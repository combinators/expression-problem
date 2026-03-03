package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

class NeedlemanWunschSequenceAlignment {
  def model: EnhancedModel = {
    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)

    val s1 = new ArgExpression(0, "s1", StringType(), "r")
    val s2 = new ArgExpression(1, "s2", StringType(), "c")
    val matchBonus = new ArgExpression(2, "matchBonus", IntegerType(), "")
    val mismatchPenalty = new ArgExpression(3, "mismatchPenalty", IntegerType(), "")
    val gapPenalty = new ArgExpression(4, "gapPenalty", IntegerType(), "")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= new StringLengthExpression(s1), new StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= new StringLengthExpression(s2), new StringLengthExpression(s2) + one)

    val score = new TernaryExpression(
      new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
      matchBonus,
      mismatchPenalty
    )

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val subproblemTraversal = ExpressionDefinition(
      new MaxExpression(
        new SubproblemExpression(Seq(r - one, c - one)) + score, // todo: add parentheses around ternary expression
        new MaxExpression(
          new SubproblemExpression(Seq(r - one, c)) + gapPenalty,
          new SubproblemExpression(Seq(r, c - one)) + gapPenalty
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
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new StringLengthExpression(s1), new StringLengthExpression(s2))))
    )

    NWSA
  }
}
