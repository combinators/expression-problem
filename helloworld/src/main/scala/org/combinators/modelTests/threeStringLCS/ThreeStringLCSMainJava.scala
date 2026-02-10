package org.combinators.modelTests.threeStringLCS

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.GenerationOption
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.model.models.twoSequences.NeedlemanWunschSequenceAlignmentModel
import org.combinators.dp.{BottomUp, TopDown}
import org.combinators.model.models.other.ThreeStringLCSModel
import org.combinators.model.Model

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class ThereStringLCSMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = ThreeStringLCSProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, model:Model, option:GenerationOption): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doublesInMethod.enable()
          _ <- generator.realDoublesInMethod.enable()
          _ <- generator.intsInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable()     // should be array, but this still needs to be added as an FFI
          _ <- generator.consoleInMethod.enable()
          _ <- generator.arraysInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()

          // HERE you can finally specify the method to use for testing and the test cases
          _ <- dpApproach.implement(model, option)
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

  def runDirectToDisc(targetDirectory: Path, model:Model, option:GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, model, option)
    } yield ExitCode.Success
  }
}

object TSLCSDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "tslcs")

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = new TopDown()
    val topDownWithMemo = new TopDown(memo = true)
    val bottomUp        = new BottomUp()

    val tslcs = new ThreeStringLCSModel().instantiate()
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new ThereStringLCSMainJava() }
      _ <- IO { println("[OK]") }

      // pass in TOP DOWN

      result <- main.runDirectToDisc(targetDirectory, tslcs, bottomUp)
    } yield result
  }
}
