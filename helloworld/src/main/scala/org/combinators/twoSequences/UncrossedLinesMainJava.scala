package org.combinators.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.model.{ArgExpression, ArrayElementExpression, EnhancedModel, ExpressionDefinition, ExpressionStatement, HelperExpression, IfThenElseDefinition, IntegerArrayType, IntegerType, LiteralArrayPair, LiteralInt, MaxExpression, SelfExpression, StringLengthExpression, SubproblemExpression, SubproblemInvocation, UnitExpression}
import java.nio.file.{Path, Paths}

class UncrossedLinesMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("ul1", new LiteralArrayPair(Array(1, 4, 2), Array(1, 2, 4)), new LiteralInt(2), new UnitExpression)
  )
}

object UncrossedLinesDirectDiskToMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "ul")

  def model: EnhancedModel = {
    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)

    val nums1 = new ArgExpression(0, "nums1", IntegerArrayType(), "r")
    val nums2 = new ArgExpression(1, "nums2", IntegerArrayType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= new StringLengthExpression(nums1), new StringLengthExpression(nums1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= new StringLengthExpression(nums2), new StringLengthExpression(nums2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val subproblemTraversal = IfThenElseDefinition(
      new ArrayElementExpression(nums1, r - one) == new ArrayElementExpression(nums2, c - one),
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
      subproblemTraversal
    )

    val UL: EnhancedModel = new EnhancedModel(
      "UncrossedLines",
      List(nums1, nums2),
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      soln,
      definition,
      answer = new SubproblemExpression(Seq(new StringLengthExpression(nums1), new StringLengthExpression(nums2)))
    )

    UL
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
      main <- IO { new UncrossedLinesMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}