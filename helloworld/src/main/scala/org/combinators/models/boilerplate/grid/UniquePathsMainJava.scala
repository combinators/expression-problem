package org.combinators.models.boilerplate.grid

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.models._
import org.combinators.models.enhancedModels.grid.UniquePaths

import java.nio.file.{Path, Paths}

class UniquePathsMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("up1", new LiteralPair(3, 7), new LiteralInt(28), new UnitExpression),
    new TestExample("up2", new LiteralPair(3, 2), new LiteralInt(3), new UnitExpression),
  )
}

object UniquePathsToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "uniquePaths")

  val model: EnhancedModel = new UniquePaths().model

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
      main <- IO { new UniquePathsMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}