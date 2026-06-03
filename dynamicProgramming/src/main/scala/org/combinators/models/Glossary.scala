package org.combinators.models

/**
 * sbt "dp/runMain org.combinators.modelTests.Glossary [bottomUp | topDown | topDownMemo]"
 *
 * Creates output files in target/bottomUp or target\topDown or target\topDownMemo
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.archive.cogen.bottomUp.twoSequences.longestCommonSubsequence.LCSMainJava
import org.combinators.archive.unenhancedModels.boilerplate.uncrossedLines.UnenhancedUncrossedLinesMainJava
import org.combinators.archive.unenhancedModels.models.twoSequences.{LongestCommonSubsequenceModel, UncrossedLinesModel}
import org.combinators.dp.enhanced.EnhancedMainInterface
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable.*
import org.combinators.boilerplate.grid.{CountSquaresMainJava, MinPathSumMainJava, UniquePathsMainJava}
import org.combinators.boilerplate.integer.{BellNumberMainJava, DiceThrowMainJava, FibonacciMainJava, PerfectSquaresMainJava, TribonacciMainJava}
import org.combinators.boilerplate.oneSequence.{MatrixChainMultiplicationBottomUpMainJava, MatrixChainMultiplicationTopDownMainJava, MinCostClimbingStairMainJava}
import org.combinators.boilerplate.strings.{InterleaveStringsMainJava, ThreeStringsLCSMainJava}
import org.combinators.boilerplate.twoSequences.{LongestCommonSubsequenceMainJava, UncrossedLinesMainJava}
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}

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
  val known_enhanced_solutions:Seq[(EnhancedMainInterface, Seq[GenerationOption])] = Seq(
    (new BellNumberMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new CountSquaresMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new DiceThrowMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new FibonacciMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new InterleaveStringsMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new LongestCommonSubsequenceMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new MinCostClimbingStairMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new PerfectSquaresMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new ThreeStringsLCSMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new UncrossedLinesMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
//    (new NeedlemanWunschSequenceAlignmentMainJava(), NeedlemanWunschSequenceAlignmentToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
//    (new DistinctSubsequencesMainJava(), DistinctSubsequencesToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
//    (new WildcardPatternMatchingMainJava(), WildcardPatternMatchingToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
//    (new ShortestCommonSupersequenceMainJava(), ShortestCommonSupersequenceToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
    (new TribonacciMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new UniquePathsMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),
    (new MinPathSumMainJava(), Seq(topDown, topDownWithMemo, bottomUp)),

    // generates but has flawed logic because of the transformation of (r,c) into (i,j)
    // CHALLENGING (new MatrixChainMultiplicationMainJava(), MatrixChainMultiplicationMainDirectToDiskMain.model, Seq(topDown, topDownWithMemo, bottomUp)),
  )

  // below are the individual DP problems generated and added to `all_files`.
  def top_down_memo_files(): Seq[FileWithPath]  = {

    // UncrossedLinesMainJava and LCSMainJava still don't work

    val mcm_td = (new MatrixChainMultiplicationTopDownMainJava(), Seq(topDown, topDownWithMemo))
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

    val mcm_td = (new MatrixChainMultiplicationTopDownMainJava(), Seq(topDown, topDownWithMemo))
    val just_td = Seq(mcm_td)

    val others = (just_td ++ known_enhanced_solutions).filter(pair
      => pair._2.contains(TopDown()))
      .flatMap(pair => pair._1.filesToGenerate(TopDown()))

    others
  }

  def bottom_up_files() : Seq[FileWithPath] = {
    val ul = new UnenhancedUncrossedLinesMainJava().filesToGenerate(new UncrossedLinesModel().instantiate(), BottomUp())
    val lcs = new LCSMainJava().filesToGenerate(new LongestCommonSubsequenceModel().instantiate(), BottomUp())

    val mcm_bot = (new MatrixChainMultiplicationBottomUpMainJava(), Seq(bottomUp))
    val just_bot = Seq(mcm_bot)

    val others = (just_bot ++ known_enhanced_solutions).filter(pair
      => pair._2.contains(BottomUp()))
      .flatMap(pair => pair._1.filesToGenerate(BottomUp()))
    others
  }

  def run(args: List[String]): IO[ExitCode] = {
    val choice = if (args.isEmpty) {
      topDownWithMemo                    // <------ CHANGE this manually when you run, to generate topDown or topDownWithMemo -- BOTTOMUP NOT YET WORKING
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
