package org.combinators.modelTests.oneSequence

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.model._
import org.combinators.model.enhancedModels.JumpGame

import java.nio.file.{Path, Paths}

/**
 * All that is needed here is the set of test cases that you need.
 */
class JumpGameMainJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("ts1", new LiteralArray(Array(1,100,1,1,1,100,1,1,100,1)), new LiteralInt(6), new UnitExpression),
    new TestExample("ts2", new LiteralArray(Array(10,15,20)), new LiteralInt(15), new UnitExpression),
  )
}

object JumpGameToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "JumpGame")

  val model: EnhancedModel = new JumpGame().model

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
      bottomUp
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MinCostClimbingStairMain() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
