package org.combinators.modelTests

/**
 * sbt "dp/runMain org.combinators.modelTests.Glossary [bottomUp | topDown | topDownMemo]
 *
 * Creates output files in target/bottomUp or target\topDown or target\topDownMemo
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.dp.{BottomUp, GenerationOption, LCSMainJava, TopDown}
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.model.models.twoSequences.{LongestCommonSubsequenceModel, UncrossedLinesModel}
import org.combinators.modelTests.uncrossedLines.UncrossedLinesMainJava
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.integer.perfectsquare.{PerfectSquareMainDirectToDiskMain, PerfectSquareMainJava}
import org.combinators.oneSequence.matrixchainmutiplication.{MatrixChainMultiplicationMainDirectToDiskMain, MatrixChainMultiplicationMainJava}
import org.combinators.strings.{InterleaveStringsMainJava, InterleaveStringsToDiskMain}

import java.nio.file.{Path, Paths}
import scala.collection.Seq

class Glossary {

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, files: Seq[FileWithPath]): IO[Unit] = {
    IO {
      print("Computing Files...")
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      files.foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path,files: Seq[FileWithPath]): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, files)
    } yield ExitCode.Success
  }
}

object GlossaryToDiskMain extends IOApp {

  // choose one of these to pass in
  val topDown         = new TopDown()
  val topDownWithMemo = new TopDown(memo = true)
  val bottomUp        = new BottomUp()

  // below are the individual DP problems generated and added to `all_files`.
  def files(choice:GenerationOption) = {
    val ps = new PerfectSquareMainJava().filesToGenerate(PerfectSquareMainDirectToDiskMain.model, choice)
    val mcm = new MatrixChainMultiplicationMainJava().filesToGenerate(MatrixChainMultiplicationMainDirectToDiskMain.model, choice)
    val ils = new InterleaveStringsMainJava().filesToGenerate(InterleaveStringsToDiskMain.model, choice)

    if (choice == topDown) {
      val ul = new UncrossedLinesMainJava().filesToGenerate(new UncrossedLinesModel().instantiate(), choice)
      val lcs = new LCSMainJava().filesToGenerate(new LongestCommonSubsequenceModel().instantiate(), choice)
      ps ++ mcm ++ ils ++ ul ++ lcs
    } else {
      // add them all together BUT ONLY for topDown
      ps ++ mcm ++ ils
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    val choice = if (args.isEmpty) {
      topDown                    // <------ CHANGE this manually when you run, to generate topDown or topDownWithMemo -- BOTTOMUP NOT YET WORKING
    } else {
      args(0).toLowerCase match {
        case "topdown" => topDown
        case "bottomup" => bottomUp
        case "topdownmemo" => topDownWithMemo
        case _ =>
          print (s"Unknown option: ${args(0)}. Must be either 'topDown', 'bottomUp' or 'topDownMemo'.")
          ???
      }
    }

    val targetDirectory:Path = Paths.get("target", choice.name)

    for {
        _ <- IO {
          println("Initializing Generator...")
          println(s"Output will appear in: ${targetDirectory}")
        }
        main <- IO { new Glossary() }

        result <- main.runDirectToDisc(targetDirectory, files(choice))

        _ <- IO { println("Make sure you run the scripts to 'fix' the generated code.") }
      } yield result
    }
}
