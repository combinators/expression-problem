package org.combinators.modelTests.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.model._
import org.combinators.model.enhancedModels.WildcardPatternMatching

import java.nio.file.{Path, Paths}

class WildcardPatternMatchingMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("wpm1", new LiteralStringPair("a*b*c", "afhuirbfhwnkc"),    new LiteralBoolean(true), new UnitExpression),
    new TestExample("wpm2", new LiteralStringPair("a?b?c", "a"),    new LiteralBoolean(false),  new UnitExpression),
    new TestExample("wpm4", new LiteralStringPair("adceb", "*a*b"), new LiteralBoolean(true),  new UnitExpression),
    new TestExample("wpm5", new LiteralStringPair("", "*"),    new LiteralBoolean(true),  new UnitExpression),
    new TestExample("wpm6", new LiteralStringPair("", ""),     new LiteralBoolean(true),  new UnitExpression),
  )
}

object WildcardPatternMatchingToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "wildcardPatternMatching")

  val model: EnhancedModel = new WildcardPatternMatching().model

  def run(args: List[String]): IO[ExitCode] = {
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

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
        new WildcardPatternMatchingMainJava()
      }
      _ <- IO {
        println("[OK]")
      }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}