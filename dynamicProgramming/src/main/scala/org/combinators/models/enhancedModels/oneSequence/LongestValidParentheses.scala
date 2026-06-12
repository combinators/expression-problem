package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._


class LongestValidParentheses {
  def model:EnhancedModel = {

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val two: LiteralInt = LiteralInt(2)

    val str = ArgExpression(0, "str", StringType(), "i")
    val bound = List(str)

    val strlen = StringLengthExpression(str)
    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= strlen - one, strlen)
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("j") <= one, two)

    val last = SubproblemExpression(Seq(i-one,zero))

    val cond = CharAtExpression(str, i) == LiteralChar(')') &&
      (zero <= i - last - one) &&
      CharAtExpression(str, i - last - one) == LiteralChar('(')

    val sol = SubproblemInvocation(Seq("i", "j"), helpers = Map("i" -> i,"j" -> j))

    val ternary = TernaryExpression(zero <= (i - last - two), SubproblemExpression(Seq(i - last - two, zero)), zero)
    val complexValue = (last + two) + ternary

    val complexCase = IfThenElseDefinition(cond, ExpressionStatement(complexValue), ExpressionDefinition(zero))
    val maxFinder = MaxExpression(SubproblemExpression(Seq(i, zero)), SubproblemExpression(Seq(i - one, one)))
    val maxCase = IfThenElseDefinition(j == one, ExpressionStatement(maxFinder), complexCase)
    val zeroCase = IfThenElseDefinition(i == zero, ExpressionStatement(zero), maxCase)

    val Fib = new EnhancedModel("LVP",
      bound,
      subproblemType = IntegerType(),    // helper methods and intermediate problems are int
      solutionType = StringType(),  // how a solution is represented (not yet effective)
      sol,
      zeroCase,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(StringLengthExpression(str) - one, one))))

    Fib
  }
}
