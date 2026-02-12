package org.combinators.modelTests

/**
 * sbt "dp/runMain org.combinators.modelTests.Glossary [bottomUp | topDown | topDownMemo]
 *
 * Creates output files in target/bottomUp or target\topDown or target\topDownMemo
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, GenerationOption, LCSMainJava, TopDown}
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.model.models.twoSequences.{LongestCommonSubsequenceModel, NeedlemanWunschSequenceAlignmentModel, UncrossedLinesModel}
import org.combinators.modelTests.uncrossedLines.UncrossedLinesMainJava
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.integer.perfectsquare.{PerfectSquareMainDirectToDiskMain, PerfectSquareMainJava}
import org.combinators.model.EnhancedModel
import org.combinators.model.models.knapsack.KnapsackModel
import org.combinators.modelTests.knapsack.KnapsackMainJava
import org.combinators.modelTests.nwsa.NWSAMainJava
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
  val topDown         = TopDown()
  val topDownWithMemo = TopDown(memo = true)
  val bottomUp        = BottomUp()

  // declare the working versions for each problem in the enhanced list
  val known_enhanced_solutions:Seq[(EnhancedDPMainJava, EnhancedModel, Seq[GenerationOption])] = Seq(
    (new PerfectSquareMainJava(), PerfectSquareMainDirectToDiskMain.model, Seq(topDown, topDownWithMemo /* NO BotUp */)),
    (new InterleaveStringsMainJava(), InterleaveStringsToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),

    // generates but has flawed logic because of the transformation of (r,c) into (i,j)
    (new MatrixChainMultiplicationMainJava(), MatrixChainMultiplicationMainDirectToDiskMain.model, Seq(topDown, topDownWithMemo /* NO BotUp */)),

  )

  // below are the individual DP problems generated and added to `all_files`.
  def top_down_memo_files() = {

    // UncrossedLinesMainJava and LCSMainJava still don't work

    val others = known_enhanced_solutions.filter(triple
      => triple._3.contains(TopDown(memo = true))).
      flatMap(triple => triple._1.filesToGenerate(triple._2, TopDown(memo = true)))

    others
  }

  // below are the individual DP problems generated and added to `all_files`.
  def top_down() = {
    val ul = new UncrossedLinesMainJava().filesToGenerate(new UncrossedLinesModel().instantiate(), TopDown())
    val lcs = new LCSMainJava().filesToGenerate(new LongestCommonSubsequenceModel().instantiate(), TopDown())
    val kp = new KnapsackMainJava().filesToGenerate(new KnapsackModel().instantiate(), TopDown())
    val nwsa = new NWSAMainJava().filesToGenerate(new NeedlemanWunschSequenceAlignmentModel().instantiate(), TopDown())

    val others = known_enhanced_solutions.filter(triple
      => triple._3.contains(TopDown())).
      flatMap(triple => triple._1.filesToGenerate(triple._2, TopDown()))

    others ++ lcs ++ ul ++ kp ++ nwsa
  }

  def bottom_up_files() = {
    val ul = new UncrossedLinesMainJava().filesToGenerate(new UncrossedLinesModel().instantiate(), BottomUp())
    val lcs = new LCSMainJava().filesToGenerate(new LongestCommonSubsequenceModel().instantiate(), BottomUp())

    val others = known_enhanced_solutions.filter(triple
      => triple._3.contains(BottomUp())).
      flatMap(triple => triple._1.filesToGenerate(triple._2, BottomUp()))

    others ++ lcs ++ ul
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

        result <- if (choice == topDown) {
          main.runDirectToDisc(targetDirectory, top_down())
        } else if (choice == topDownWithMemo) {
          main.runDirectToDisc(targetDirectory, top_down_memo_files())
        } else {
          main.runDirectToDisc(targetDirectory, bottom_up_files())
        }

        _ <- IO { println("Make sure you run the scripts to 'fix' the generated code.") }
      } yield result
    }
}
