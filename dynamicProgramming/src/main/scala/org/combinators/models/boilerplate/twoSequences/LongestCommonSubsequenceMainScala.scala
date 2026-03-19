package org.combinators.models.boilerplate.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainScala
import org.combinators.models._
import org.combinators.models.enhancedModels.twoSequences.LongestCommonSubsequence

import java.nio.file.{Path, Paths}

class LongestCommonSubsequenceMainScala extends EnhancedDPMainScala {
  override def tests = Seq(
    new TestExample("lcs1", new LiteralStringPair("abc", "ace"), new LiteralInt(2), new UnitExpression),
  )
}

object LongestCommonSubsequenceToDiskScalaMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "longestCommonSubsequence")

  val model: EnhancedModel = new LongestCommonSubsequence().model

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
        new LongestCommonSubsequenceMainScala()
      }
      _ <- IO {
        println("[OK]")
      }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}

