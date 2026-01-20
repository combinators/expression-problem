package org.combinators.bottomUp.twoSequences.longestCommonSubsequence

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.model.{CharAtExpression, EqualExpression, IntegerExpression, IteratorExpression, LiteralInt, Model, StringExpression}

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class LongestCommonSubsequenceMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("world"))))

  val dpApproach = LongestCommonSubsequenceObjectOrientedProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.ooParadigm, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.equalityInMethod)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, model:Model): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doublesInMethod.enable()
          _ <- generator.intsInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable()     // should be array, but this still needs to be added as an FFI
          _ <- generator.consoleInMethod.enable()
          _ <- generator.arraysInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()

          _ <- dpApproach.implement(model:Model)
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

  def runDirectToDisc(targetDirectory: Path, model:Model): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, model)
    } yield ExitCode.Success
  }
}

object LongestCommonSubsequenceDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "bottomUp", "twoSequences", "longestcommonsubsequence")

  def run(args: List[String]): IO[ExitCode] = {
    val LCS: Model = new Model("LongestCommonSubsequence", List(), List())
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new LongestCommonSubsequenceMainJava() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory, LCS)
    } yield result
  }
}
