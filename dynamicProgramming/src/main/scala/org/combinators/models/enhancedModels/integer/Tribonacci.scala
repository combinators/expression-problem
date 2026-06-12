package org.combinators.models.enhancedModels.integer

import org.combinators.models._

class Tribonacci {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)
    val two = LiteralInt(2)
    val three = LiteralInt(3)

    val n = ArgExpression(0, "n", IntegerType(), "i")
    val bound = List(n)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= n, n + one)

    val sol = SubproblemInvocation(Seq("i"), helpers = Map("i" -> i))

    val twoCase = IfThenElseDefinition(
      i == two,
      ExpressionStatement(one),
      ExpressionDefinition(
        SubproblemExpression(Seq(i - one)) +
          SubproblemExpression(Seq(i - two)) +
          SubproblemExpression(Seq(i - three))
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
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(n)))
    )

    Trib
  }
}