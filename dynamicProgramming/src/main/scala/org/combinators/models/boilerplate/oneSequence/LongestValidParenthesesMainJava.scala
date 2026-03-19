package org.combinators.models.boilerplate.oneSequence

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models._
import org.combinators.models.enhancedModels.oneSequence.LongestValidParentheses

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class LongestValidParenthesesMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    /** https://leetcode.com/problems/longest-valid-parentheses */
    new TestExample("lvp1", new LiteralString(")()())"), new LiteralInt(4), new UnitExpression),

    /** https://leetcode.com/problems/longest-valid-parentheses/solutions/14133/my-dp-on-solution-without-using-stack-by-nsyp/ */
    new TestExample("lvp2", new LiteralString("()(())"), new LiteralInt(6), new UnitExpression),
  )
}

object LongestValidParenthesesDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "LVP")

  val model: EnhancedModel = new LongestValidParentheses().model

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

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
      main <- IO { new LongestValidParenthesesMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, LongestValidParenthesesDirectToDiskMain.model, choice)
    } yield result
  }
}
