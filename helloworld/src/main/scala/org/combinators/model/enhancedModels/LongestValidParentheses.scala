package org.combinators.model.enhancedModels

import org.combinators.model._


class LongestValidParentheses {
  def model:EnhancedModel = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    val str = new ArgExpression(0, "str", StringType(), "i")
    val bound = List(str)


    val strlen = new StringLengthExpression(str)
    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= strlen, strlen  + one) // only one argument, i

    val last = new SubproblemExpression(Seq(i-one))

    val cond = new CharAtExpression(str,i)== new LiteralChar(')') &&
      (zero<=i-last-one) &&
      new CharAtExpression(str, i-last-one)== new LiteralChar('(')

    val sol = SubproblemInvocation(Seq("i"), helpers = Map("i" -> i))

    val complexValue = last + two + new TernaryExpression(zero<=i-last-two,new SubproblemExpression(Seq(i-last-two)),zero)

    val complexCase = IfThenElseDefinition(cond, ExpressionStatement(complexValue), ExpressionDefinition(zero))
    val zeroCase = IfThenElseDefinition(i == zero, ExpressionStatement(zero), complexCase)


    val Fib = new EnhancedModel("LVP",
      bound,
      subproblemType = IntegerType(),    // helper methods and intermediate problems are int
      solutionType = StringType(),  // how a solution is represented (not yet effective)
      sol,
      zeroCase,
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new StringLengthExpression(str)))))

    Fib
  }
}
