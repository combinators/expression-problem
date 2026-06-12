package org.combinators.archive.cogen.bottomUp.twoSequences.longestCommonSubsequence

/**
 * One of the earliest implementations to generate bottom-up implementation of Longest Common Subsequence
 *
 * val targetDirectory = Paths.get("target", "pascalBU")
 */
import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.archive.unenhancedModels.models.twoSequences.LongestCommonSubsequenceModel
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable._
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Syntax, Unboxed}
import org.combinators.models.original.Model

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class LCSMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = LCSProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  def filesToGenerate(model:Model, option:GenerationOption):Seq[FileWithPath] = {
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

        // HERE you can finally specify the method to use for testing and the test cases
        _ <- dpApproach.implement(model, option)
      } yield ()
    }
  }

  def directToDiskTransaction(targetDirectory: Path, model:Model, option:GenerationOption): IO[Unit] = {

     IO {
      println("Computing Files...")
      val computed = filesToGenerate(model, option)
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        println(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      println("Persisting Files...")
      computed.foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path, model:Model, option:GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, model, option)
    } yield ExitCode.Success
  }
}

object LCSDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "lcs")

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    val LCS = new LongestCommonSubsequenceModel().instantiate()
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new LCSMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, LCS, topDownWithMemo)
    } yield result
  }
}
