package org.combinators.models.boilerplate.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.models._
import org.combinators.models.enhancedModels.twoSequences.DistinctSubsequences

import java.nio.file.{Path, Paths}

class DistinctSubsequencesMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("ds1", new LiteralStringPair("rabbbit", "rabbit"), new LiteralInt(3), new UnitExpression),
    new TestExample("ds2", new LiteralStringPair("babgbag", "bag"),    new LiteralInt(5), new UnitExpression),
  )
}

object DistinctSubsequencesToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "distinctSubsequences")

  val model: EnhancedModel = new DistinctSubsequences().model

  def run(args: List[String]): IO[ExitCode] = {
    val topDown = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp = BottomUp()

    val choice = if (args.length == 1) {
      args(0).toLowerCase() match {
        case "topdown"         => topDown
        case "topdownwithmemo" => topDownWithMemo
        case "bottomup"        => bottomUp
        case _                 => ???
      }
    } else {
      bottomUp
    }

    for {
      _ <- IO {
        print("Initializing Generator...")
      }
      main <- IO {
        new DistinctSubsequencesMainJava()
      }
      _ <- IO {
        println("[OK]")
      }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}