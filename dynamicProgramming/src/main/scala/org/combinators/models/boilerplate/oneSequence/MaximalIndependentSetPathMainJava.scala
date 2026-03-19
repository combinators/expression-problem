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
import org.combinators.models.enhancedModels.oneSequence.MaximalIndependentSetPath

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class MaximalIndependentSetPathMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    // https://canvas.wpi.edu/courses/79353
    new TestExample("sp1", new LiteralArray(Array(12,11,13,15)), new LiteralInt(27), new UnitExpression),
    new TestExample("sp2", new LiteralArray(Array(2,1000,3,1)), new LiteralInt(1001), new UnitExpression),
  )
}

object MaximalIndependentSetPathDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "MISP")

  val model: EnhancedModel = new MaximalIndependentSetPath().model

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
      bottomUp
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MaximalIndependentSetPathMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, MaximalIndependentSetPathDirectToDiskMain.model, choice)
    } yield result
  }
}
