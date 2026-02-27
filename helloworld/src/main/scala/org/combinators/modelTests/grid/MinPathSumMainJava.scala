package org.combinators.modelTests.grid

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.model._
import org.combinators.model.enhancedModels.MinPathSum

import java.nio.file.{Path, Paths}

class MinPathSumMainJava extends EnhancedDPMainJava {
  override def tests = Seq( // todo: replace with actual test cases
    new TestExample("up1", new LiteralPair(3, 7), new LiteralInt(28), new UnitExpression),
    new TestExample("up2", new LiteralPair(3, 2), new LiteralInt(3), new UnitExpression),
  )
}

object MinPathSumToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "minPathSum")

  val model: EnhancedModel = new MinPathSum().model

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
      _ <- IO {
        print("Initializing Generator...")
      }
      main <- IO {
        new MinPathSumMainJava()
      }
      _ <- IO {
        println("[OK]")
      }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}