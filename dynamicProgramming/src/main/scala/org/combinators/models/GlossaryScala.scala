package org.combinators.models

/**
 * sbt "dp/runMain org.combinators.modelTests.Glossary [bottomUp | topDown | topDownMemo]
 *
 * Creates output files in target/bottomUp or target\topDown or target\topDownMemo
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.archive.cogen.bottomUp.twoSequences.longestCommonSubsequence.LCSMainJava
import org.combinators.archive.unenhancedModels.boilerplate.unenhancedUncrossedLines.UnenhancedUncrossedLinesMainJava
import org.combinators.archive.unenhancedModels.models.twoSequences.{LongestCommonSubsequenceModel, UncrossedLinesModel}
import org.combinators.dp.enhanced.EnhancedMainInterface
import org.combinators.dp.{BottomUp, GenerationOption, TopDown}
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable._
import org.combinators.models.boilerplate.grid._
import org.combinators.models.boilerplate.integer._
import org.combinators.models.boilerplate.oneSequence._
import org.combinators.models.boilerplate.strings._
import org.combinators.models.boilerplate.twoSequences._

import java.nio.file.{Path, Paths}
import scala.collection.Seq

class GlossaryScala {

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

object GlossaryScalaToDiskMain extends IOApp {

  // choose one of these to pass in
  val topDown         = TopDown()
  val topDownWithMemo = TopDown(memo = true)
  val bottomUp        = BottomUp()

  // declare the working versions for each problem in the enhanced list
  val known_enhanced_solutions:Seq[(EnhancedMainInterface, Seq[GenerationOption])] = Seq(
    (new BellNumberMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new CountSquaresMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new DiceThrowMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new FibonacciMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new InterleaveStringsMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new LongestCommonSubsequenceMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new MinCostClimbingStairMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new PerfectSquaresMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new ThreeStringsLCSMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new UncrossedLinesMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
//    (new NeedlemanWunschSequenceAlignmentMainJava(), NeedlemanWunschSequenceAlignmentToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
//    (new DistinctSubsequencesMainJava(), DistinctSubsequencesToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
//    (new WildcardPatternMatchingMainJava(), WildcardPatternMatchingToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
//    (new ShortestCommonSupersequenceMainJava(), ShortestCommonSupersequenceToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
    (new TribonacciMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new UniquePathsMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new MinPathSumMainScala(), Seq(topDown, topDownWithMemo, bottomUp)),

    // generates but has flawed logic because of the transformation of (r,c) into (i,j)
    // CHALLENGING (new MatrixChainMultiplicationMainJava(), MatrixChainMultiplicationMainDirectToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
  )

  // below are the individual DP problems generated and added to `all_files`.
  def top_down_memo_files(): Seq[FileWithPath]  = {

    // UncrossedLinesMainJava and LCSMainJava still don't work

    val mcm_td = (new MatrixChainMultiplicationTopDownMainScala(), Seq(topDown, topDownWithMemo))
    val just_td = Seq(mcm_td)

    val others = (just_td ++ known_enhanced_solutions).filter(pair
      => pair._2.contains(TopDown(memo = true))).
      flatMap(pair => pair._1.filesToGenerate(TopDown(memo = true)))

    others
  }

  // below are the individual DP problems generated and added to `all_files`.
  def top_down(): Seq[FileWithPath]  = {
//    val ul = new UncrossedLinesMainJava().filesToGenerate(new UncrossedLinesModel().instantiate(), TopDown())
    //val lcs = new LCSMainJava().filesToGenerate(new LongestCommonSubsequenceModel().instantiate(), TopDown())              [HEINEMAN: not working]
    // val kp = new KnapsackMainJava().filesToGenerate(new KnapsackModel().instantiate(), TopDown())                         [HEINEMAN: not working]
    //val nwsa = new NWSAMainJava().filesToGenerate(new NeedlemanWunschSequenceAlignmentModel().instantiate(), TopDown())    [HEINEMAN: not working]

    val mcm_td = (new MatrixChainMultiplicationTopDownMainScala(), Seq(topDown, topDownWithMemo))
    val just_td = Seq(mcm_td)

    val others = (just_td ++ known_enhanced_solutions).filter(pair
      => pair._2.contains(TopDown())).
      flatMap(pair => pair._1.filesToGenerate(TopDown()))

    others
  }

  def bottom_up_files() : Seq[FileWithPath] = {
//    val ul = new UnenhancedUncrossedLinesMainJava().filesToGenerate(new UncrossedLinesModel().instantiate(), BottomUp())
//    val lcs = new LCSMainJava().filesToGenerate(new LongestCommonSubsequenceModel().instantiate(), BottomUp())

    val mcm_bot = (new MatrixChainMultiplicationBottomUpMainScala(), Seq(bottomUp))
    val just_bot = Seq(mcm_bot)

    val others =  (just_bot ++ known_enhanced_solutions).filter(pair
      => pair._2.contains(BottomUp())).
      flatMap(pair => pair._1.filesToGenerate(BottomUp()))

    others
  }

  def run(args: List[String]): IO[ExitCode] = {
    val choice = if (args.isEmpty) {
      bottomUp                    // <------ CHANGE this manually when you run, to generate topDown or topDownWithMemo -- BOTTOMUP NOT YET WORKING
    } else {
      args(0).toLowerCase match {
        case "topdown" => topDown
        case "bottomup" => bottomUp
        case "topdownmemo" => topDownWithMemo
        case "topdownwithmemo" => topDownWithMemo
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
