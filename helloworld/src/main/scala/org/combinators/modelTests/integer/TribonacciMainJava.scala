package org.combinators.modelTests.integer

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.model._
import org.combinators.model.enhancedModels.Tribonacci

import java.nio.file.{Path, Paths}

class TribonacciMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("trib0", new LiteralInt(0), new LiteralInt(0), new UnitExpression),
    new TestExample("trib1", new LiteralInt(1), new LiteralInt(1), new UnitExpression),
    new TestExample("trib2", new LiteralInt(2), new LiteralInt(1), new UnitExpression),
    new TestExample("trib3", new LiteralInt(3), new LiteralInt(2), new UnitExpression),
    new TestExample("trib4", new LiteralInt(4), new LiteralInt(4), new UnitExpression),
    new TestExample("trib5", new LiteralInt(5), new LiteralInt(7), new UnitExpression),
  )
}

object TribonacciToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "tribonacci")

  val model: EnhancedModel = new Tribonacci().model

  def run(args: List[String]): IO[ExitCode] = {
    val topDown = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp = BottomUp()

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
      main <- IO { new TribonacciMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}