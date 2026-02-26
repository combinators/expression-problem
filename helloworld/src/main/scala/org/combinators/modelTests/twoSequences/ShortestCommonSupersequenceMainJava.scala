package org.combinators.modelTests.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.model._
import org.combinators.model.enhancedModels.ShortestCommonSupersequence

import java.nio.file.{Path, Paths}

class ShortestCommonSupersequenceMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("scs1", new LiteralStringPair("abac", "cab"), new LiteralInt(5), new UnitExpression),
    new TestExample("scs2", new LiteralStringPair("abc", "ac"), new LiteralInt(4), new UnitExpression),
    new TestExample("scs3", new LiteralStringPair("abc", "abc"), new LiteralInt(3), new UnitExpression),
    new TestExample("scs4", new LiteralStringPair("", "abc"), new LiteralInt(3), new UnitExpression),
    new TestExample("scs5", new LiteralStringPair("abc", ""), new LiteralInt(3), new UnitExpression),
  )
}

object ShortestCommonSupersequenceToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "shortestCommonSupersequence")

  val model: EnhancedModel = new ShortestCommonSupersequence().model

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
        new ShortestCommonSupersequenceMainJava()
      }
      _ <- IO {
        println("[OK]")
      }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}