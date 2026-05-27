package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

// todo: identify root cause of test case error
class NeedlemanWunschSequenceAlignment {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)

    val s1 = ArgExpression(0, "s1", StringType(), "r")
    val s2 = ArgExpression(1, "s2", StringType(), "c")
    val matchBonus = ArgExpression(2, "matchBonus", IntegerType(), "")
    val mismatchPenalty = ArgExpression(3, "mismatchPenalty", IntegerType(), "")
    val gapPenalty = ArgExpression(4, "gapPenalty", IntegerType(), "")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= StringLengthExpression(s1), StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= StringLengthExpression(s2), StringLengthExpression(s2) + one)

    val score = TernaryExpression(
      CharAtExpression(s1, r - one) == CharAtExpression(s2, c - one),
      matchBonus,
      mismatchPenalty
    )

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val subproblemTraversal = ExpressionDefinition(
      MaxExpression(
        SubproblemExpression(Seq(r - one, c - one)) + score, 
        MaxExpression(
          SubproblemExpression(Seq(r - one, c)) + gapPenalty,
          SubproblemExpression(Seq(r, c - one)) + gapPenalty
        )
      )

    )

    val baseCase2 = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(r * gapPenalty),
      subproblemTraversal
    )

    val definition = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(c * gapPenalty),
      baseCase2
    )

    val NWSA: EnhancedModel = new EnhancedModel(
      "NeedlemanWunschSequenceAlignment",
      List(s1, s2, matchBonus, mismatchPenalty, gapPenalty),
      subproblemType = IntegerType(),
      solutionType = StringType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(StringLengthExpression(s1), StringLengthExpression(s2))))
    )

    NWSA
  }
}
