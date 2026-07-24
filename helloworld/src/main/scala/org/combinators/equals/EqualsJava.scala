package org.combinators.equals

/**
 * Code for generating Equals() method for a CompositeDataType.
 *
 * Note: compare with built-in Eclipse code (Java Development Tools, jdt) that generates hashCode() or equals() method.
 *
 * https://github.com/eclipse-jdt/eclipse.jdt.ui/blob/11c5a251b40428f84964dc0c3f002e2b3607be2e/org.eclipse.jdt.core.manipulation/core%20extension/org/eclipse/jdt/internal/corext/codemanipulation/GenerateHashCodeEqualsOperation.java#L845
 *
 * GenerateHashCodeEqualsDialog
 *
 * GenerateHashCodeEqualsOperation
 *
 */
import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable, NameProvider, TypeRep}
import FileWithPathPersistable.*
import com.github.javaparser.{JavaParser, StaticJavaParser}
import org.combinators.ep.language.java.Syntax.MangledName
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, Syntax, Unboxed}
import org.combinators.equals.ffi.java.BaseType

import java.nio.file.{Path, Paths}
import scala.util.Try

/**
 * Java language backend is not built to allow for customized updates, so this simply provides an
 * alternate implementation.
 */
object AllowEqualsJavaNameProvider extends NameProvider[MangledName] {
  val parser = new JavaParser(StaticJavaParser.getParserConfiguration)

  /** Need to have single-param version so this can be used in map. */
  def mangle(name: String): MangledName = {
    mangle(name, Set("Object", "hashCode", "toString", "getClass"))
  }

  /** Tries to parse names as a
   * [[https://docs.oracle.com/javase/specs/jls/se7/html/jls-6.html#jls-6.2 simple Java name]] and mangles to
   * the arabic number representation of the UTF-8 bytes in the given string, where each byte is prefixed by "_".
   *
   * Example:
   * {{
   * JavaNameProvider.mangle("foo") // returns "foo"
   * JavaNameProvider.mangle("class") // returns "_99_108_97_115_115" because "class" is a reserved keyword
   * }}
   */
  def mangle(name: String, forbidden:Set[String]): MangledName = {
    var cleanName = name

    // some default methods in java.lang.Object CANNOT be overridden as needed by some AIPs, so
    // take steps to avoid special java methods. To ensure 'equals' and other FFI-required names
    // go through unchanged, we allow for optional parameter to eliminate.
    while (forbidden.contains(cleanName)) {
      cleanName = "_" + cleanName
    }

    MangledName(cleanName,
      Try(parser.parseSimpleName(cleanName).getResult.map[String](_.getIdentifier).get).getOrElse {
        cleanName.getBytes(java.nio.charset.StandardCharsets.UTF_8).mkString("_", "_", "")
      }
    )
  }

  def addPrefix(prefix: String, name: MangledName): MangledName = {
    mangle(prefix + name.original)
  }

  def addSuffix(name: MangledName, suffix: String): MangledName = {
    mangle(name.original + suffix)
  }
}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class EqualsJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("eql"))))  // cannot be reserved word like 'equals'

  val baseTypesJava: BaseType.Aux[generator.paradigm.MethodBodyContext, generator.paradigm.type, generator.ooParadigm.type] = BaseType(generator.paradigm)(generator.ooParadigm)
  val eqlsGenerator : EqualsGenerator.Aux[generator.paradigm.type, generator.ooParadigm.type] = EqualsGenerator(generator.paradigm)(
    AllowEqualsJavaNameProvider, baseTypesJava, generator.imperativeInMethod, generator.equalityInMethod, generator.booleansInMethod, generator.arraysInMethod, generator.ooParadigm)
  val equalsApproach = EqualsObjectOrientedProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(AllowEqualsJavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.intsInMethod, generator.booleansInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.equalityInMethod, generator.mapsInMethod, baseTypesJava, eqlsGenerator)

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, domains:Seq[CompositeDataType],
                              testCases:Seq[EqualsTestCase]): IO[Unit] = {
    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doublesInMethod.enable()
          _ <- generator.intsInMethod.enable()
          _ <- generator.booleansInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable()
          _ <- generator.consoleInMethod.enable()
          _ <- generator.arraysInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()
          _ <- generator.mapsInMethod.enable()
          _ <- baseTypesJava.enable()

          _ <- equalsApproach.implement(domains, testCases)
        } yield ()
      }

     IO {
      print("Computing Files...")
      val computed = files()
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      files().foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path,
                      domains:Seq[CompositeDataType],
                      testCases:Seq[EqualsTestCase]
                     ): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, domains, testCases)
    } yield ExitCode.Success
  }
}

object EqualsJavaDirectToDiskMain extends IOApp {

  val targetDirectory = Paths.get("target", "eql")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new EqualsJava() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory,
        Seq(ShapeDomain.rectangle, ShapeDomain.point),
        ShapeDomain.testCases)
    } yield result
  }
}
