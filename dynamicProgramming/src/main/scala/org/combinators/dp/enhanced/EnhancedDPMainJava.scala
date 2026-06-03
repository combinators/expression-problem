package org.combinators.dp.enhanced

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.TestExample
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable.*
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax, Unboxed}
import org.combinators.models.*

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
abstract class EnhancedDPMainJava extends IOApp with EnhancedMainInterface {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = EnhancedDPObjectOrientedProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod,  generator.ooParadigm, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.mapsInMethod, generator.mapsInConstructor, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  // subclasses will provide tests and model
  def tests:Seq[TestExample]
  def model:EnhancedModel

  // subclasses describe how to instantiate desired app class
  def constructApp(): EnhancedDPMainJava
  
  def filesToGenerate(option: GenerationOption): Seq[FileWithPath] = {
    println(s"Generating ${model.problem}...")
    generator.paradigm.runGenerator {
      for {
        _ <- generator.doublesInMethod.enable()
        _ <- generator.realDoublesInMethod.enable()
        _ <- generator.intsInMethod.enable()
        _ <- generator.stringsInMethod.enable()
        _ <- generator.listsInMethod.enable()
        _ <- generator.consoleInMethod.enable()
        _ <- generator.arraysInMethod.enable()
        _ <- generator.equalityInMethod.enable()
        _ <- generator.assertionsInMethod.enable()
        _ <- generator.booleansInMethod.enable()
        _ <- generator.mapsInMethod.enable()
        _ <- generator.mapsInConstructor.enable()

        // HERE you can finally specify the method to use for testing and the test cases
        _ <- dpApproach.implement(model, tests, option)
      } yield ()
    }
  }

  def directToDiskTransaction(targetDirectory: Path, option:GenerationOption): IO[Unit] = {

     IO {
      print("Computing Files...")
      val computed = filesToGenerate(option)
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      computed.foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path, option:GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, option)
    } yield ExitCode.Success
  }

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp = BottomUp()

    val choice = if (args.length == 1) {
      args(0).toLowerCase() match {
        case "topdown" => topDown
        case "topdownwithmemo" => topDownWithMemo
        case "topdownmemo" => topDownWithMemo
        case "bottomup" => bottomUp
        case _ => ???
      }
    } else {
      bottomUp
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { constructApp() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(Paths.get("target", "java", model.problem), choice)
    } yield result
  }
}
