package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

class LongestCommonSubsequence {
  def model:EnhancedModel = {
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val s1 = ArgExpression(0, "s1", StringType(), "r")
    val s2 = ArgExpression(1, "s2", StringType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= StringLengthExpression(s1), StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= StringLengthExpression(s2), StringLengthExpression(s2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order=Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val subproblemTraversal = IfThenElseDefinition(
      CharAtExpression(s1, r - one) == CharAtExpression(s2, c - one),
      ExpressionStatement(SubproblemExpression(Seq(r - one, c - one)) + one),
      ExpressionDefinition(
        MaxExpression(
          SubproblemExpression(Seq(r, c - one)),
          SubproblemExpression(Seq(r - one, c))
        )
      )
    )

    val definition = IfThenElseDefinition(
      r == zero || c == zero,
      ExpressionStatement(zero),
      subproblemTraversal
    )

    val LCS: EnhancedModel = new EnhancedModel(
      "LongestCommonSubsequence",
      List(s1, s2),
      subproblemType = IntegerType(),
      solutionType = StringType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(StringLengthExpression(s1), StringLengthExpression(s2))))
    )

    LCS
  }
}
