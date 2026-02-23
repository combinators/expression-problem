package org.combinators.model.enhancedModels

import org.combinators.model._

class Tribonacci {
  def model: EnhancedModel = {
    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)
    val two = new LiteralInt(2)
    val three = new LiteralInt(3)

    val n = new ArgExpression(0, "n", IntegerType(), "i")
    val bound = List(n)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= n, n + one)

    val sol = SubproblemInvocation(Seq("i"), helpers = Map("i" -> i))

    val twoCase = IfThenElseDefinition(
      i == two,
      ExpressionStatement(one),
      ExpressionDefinition(
        new SubproblemExpression(Seq(i - one)) +
          new SubproblemExpression(Seq(i - two)) +
          new SubproblemExpression(Seq(i - three))
      )
    )

    val oneCase = IfThenElseDefinition(
      i == one,
      ExpressionStatement(one),
      twoCase
    )

    val definition = IfThenElseDefinition(
      i == zero,
      ExpressionStatement(zero),
      oneCase
    )

    val Trib = new EnhancedModel("Tribonacci",
      bound,
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      sol,
      definition,
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(n)))
    )

    Trib
  }
}