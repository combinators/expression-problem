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
import org.combinators.models.enhancedModels.integer.DiceThrow

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class DiceThrowMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    new TestExample("dt1", new LiteralTriple(6, 3, 12), new LiteralInt(25), new UnitExpression) //  https://www.geeksforgeeks.org/dsa/dice-throw-dp-30/
  )
}

object DiceThrowDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "diceThrow")

  val model: EnhancedModel = new DiceThrow().model

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
      main <- IO { new DiceThrowMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, DiceThrowDirectToDiskMain.model, choice)
    } yield result
  }
}
