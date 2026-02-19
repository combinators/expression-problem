package org.combinators.modelTests.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.model._
import org.combinators.model.enhancedModels.UncrossedLines

import java.nio.file.{Path, Paths}

class UncrossedLinesMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("ul1", new LiteralArrayPair(Array(1, 4, 2), Array(1, 2, 4)), new LiteralInt(2), new UnitExpression)
  )
}

object UncrossedLinesDirectDiskToMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "uncrossedLines")

  val model: EnhancedModel = new UncrossedLines().model

  def run(args: List[String]): IO[ExitCode] = {
    val topDown = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp = BottomUp()

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
      main <- IO { new UncrossedLinesMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}