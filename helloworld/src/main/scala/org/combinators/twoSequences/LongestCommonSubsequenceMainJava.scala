package org.combinators.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.model.{ArgExpression, CharAtExpression, EnhancedModel, ExpressionDefinition, ExpressionStatement, HelperExpression, IfThenElseDefinition, IntegerType, LiteralInt, LiteralStringPair, MaxExpression, SelfExpression, StringLengthExpression, StringType, SubproblemExpression, SubproblemInvocation, UnitExpression}

import java.nio.file.{Path, Paths}

class LongestCommonSubsequenceMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("lcs1", new LiteralStringPair("abc", "ace"), new LiteralInt(2), new UnitExpression),
  )
}

object LongestCommonSubsequenceDirectDiskToMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "lcs")

  def model:EnhancedModel = {
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val s1 = new ArgExpression(0, "s1", StringType(), "r")
    val s2 = new ArgExpression(0, "s2", StringType(), "c")

    val r: HelperExpression = HelperExpression("r", one, SelfExpression("r") <= new StringLengthExpression(s1), new StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", one, SelfExpression("c") <= new StringLengthExpression(s2), new StringLengthExpression(s2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order=Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val subproblemCheck = IfThenElseDefinition(
      new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
      ExpressionStatement(new SubproblemExpression(Seq(r - one, c - one)) + one),
      ExpressionDefinition(
        new MaxExpression(
          new SubproblemExpression(Seq(r, c - one)),
          new SubproblemExpression(Seq(r - one, c))
        )
      )
    )

    val definition = IfThenElseDefinition(
      r == zero || c == zero,
      ExpressionStatement(zero),
      subproblemCheck
    )

    val LCS: EnhancedModel = new EnhancedModel(
      "LongestCommonSubsequence",
      List(s1, s2),
      subproblemType = IntegerType(),
      solutionType = StringType(),
      soln,
      definition,
      answer = new SubproblemExpression(Seq(new StringLengthExpression(s1), new StringLengthExpression(s2)))
    )

    LCS
  }

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
      main <- IO { new LongestCommonSubsequenceMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}

