package org.combinators.models.boilerplate.grid

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.models._
import org.combinators.models.enhancedModels.grid.{CountSquares, MinPathSum}

import java.nio.file.{Path, Paths}

class CountSquaresMainJava extends EnhancedDPMainJava {
  override def tests = Seq( // todo: replace with actual test cases
    new TestExample("cs1", new LiteralArray(Array(0,1,1,1, 1,1,1,1, 0,1,1,1), Seq(3,4)), new LiteralInt(15), new UnitExpression)
  )
}

object CountSquaresToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "countSquares")

  val model: EnhancedModel = new CountSquares().model

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
      topDown
    }

    for {
      _ <- IO {
        print("Initializing Generator...")
      }
      main <- IO {
        new CountSquaresMainJava()
      }
      _ <- IO {
        println("[OK]")
      }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}