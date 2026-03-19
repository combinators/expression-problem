package org.combinators.models.boilerplate.oneSequence

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models._
import org.combinators.models.enhancedModels.oneSequence.LongestIncreasingSubsequence

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class LongestIncreasingSubsequenceMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    /** https://en.wikipedia.org/wiki/Longest_increasing_subsequence */
    new TestExample("lis1", new LiteralArray(Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)), new LiteralInt(6), new UnitExpression),

    /** https://www.geeksforgeeks.org/dsa/longest-increasing-subsequence-dp-3/ */
    new TestExample("lis2", new LiteralArray(Array(3, 10, 2, 1, 20)), new LiteralInt(3), new UnitExpression)
  )
}

object LongestIncreasingSubsequenceDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "LIS")

  val model: EnhancedModel = new LongestIncreasingSubsequence().model

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    val choice = if (args.length == 1) {
      args(0).toLowerCase() match {
        case "topdown" => topDown
        case "topdownwithmemo" => topDownWithMemo
        case "bottomUp" => bottomUp
        case _ => ???
      }
    } else {
      topDownWithMemo
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new LongestIncreasingSubsequenceMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, LongestIncreasingSubsequenceDirectToDiskMain.model, choice)
    } yield result
  }
}
