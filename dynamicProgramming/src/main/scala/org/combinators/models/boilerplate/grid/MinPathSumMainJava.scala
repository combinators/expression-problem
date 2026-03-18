package org.combinators.models.boilerplate.grid

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.models._
import org.combinators.models.enhancedModels.grid.MinPathSum

import java.nio.file.{Path, Paths}

class MinPathSumMainJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("mps1", new LiteralArray(Array(1,3,1, 1,5,1, 4,2,1), Seq(3,3)), new LiteralInt(7), new UnitExpression),
    new TestExample("mps2", new LiteralArray(Array(1,2,3, 4,5,6), Seq(2,3)), new LiteralInt(12), new UnitExpression),
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