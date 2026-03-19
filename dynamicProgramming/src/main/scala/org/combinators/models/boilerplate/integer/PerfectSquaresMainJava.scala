package org.combinators.models.boilerplate.integer

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models._
import org.combinators.models.enhancedModels.integer.PerfectSquares

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class PerfectSquareMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    new TestExample("ps1", new LiteralInt(13), new LiteralInt(2), new UnitExpression), // 9 + 4
    new TestExample("ps2", new LiteralInt(14), new LiteralInt(3), new UnitExpression), // 9 + 4 + 1
    new TestExample("ps3", new LiteralInt(15), new LiteralInt(4), new UnitExpression), // 9 + 4 + 1 + 1
    new TestExample("ps4", new LiteralInt(16), new LiteralInt(1), new UnitExpression), // 16
  )

}

object PerfectSquareMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "perfectSquare")

  val model: EnhancedModel = new PerfectSquares().model

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    val choice = if (args.length == 1) {
        args(0).toLowerCase() match {
          case "topdown" => topDown
          case "topdownwithmemo" => topDownWithMemo
          case "bottomup" => bottomUp
          case _ => ???
        }
    } else {
      topDown
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new PerfectSquareMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, PerfectSquareMainDirectToDiskMain.model, choice)
    } yield result
  }
}
