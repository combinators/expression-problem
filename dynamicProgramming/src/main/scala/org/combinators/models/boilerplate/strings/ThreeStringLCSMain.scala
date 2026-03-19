package org.combinators.models.boilerplate.strings

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models._
import org.combinators.models.enhancedModels.strings.ThreeStringLCS

import java.nio.file.{Path, Paths}

/**
 * All that is needed here is the set of test cases that you need.
 */
class ThreeStringsLCSMainJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("ts1", new LiteralStringTriple("AGGT12", "12TXAYB", "12XBA"), new LiteralInt(2), new UnitExpression),
    new TestExample("ts2", new LiteralStringTriple("geeks", "geeksfor", "geeksforgeeks"), new LiteralInt(5), new UnitExpression),
    new TestExample("ts3", new LiteralStringTriple("abcd1e2", "bc12ea", "bd1ea"), new LiteralInt(3), new UnitExpression),
  )
}

object ThreeStringsLCSToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "threeStringLCS")

  val model: EnhancedModel = new ThreeStringLCS().model

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
      main <- IO { new ThreeStringsLCSMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
