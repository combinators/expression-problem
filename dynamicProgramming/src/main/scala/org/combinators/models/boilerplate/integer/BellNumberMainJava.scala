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
import org.combinators.models.enhancedModels.integer.BellNumber

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class BellNumberMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    new TestExample("bn1", new LiteralInt(3), new LiteralInt(5), new UnitExpression),   // https://en.wikipedia.org/wiki/Bell_number
    new TestExample("bn2", new LiteralInt(2), new LiteralInt(2), new UnitExpression),
    new TestExample("bn3", new LiteralInt(5), new LiteralInt(52), new UnitExpression),
  )
}

object BellNumberDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "bellnumber")

  val model: EnhancedModel = new BellNumber().model

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
      main <- IO { new BellNumberMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, BellNumberDirectToDiskMain.model, choice)
    } yield result
  }
}
