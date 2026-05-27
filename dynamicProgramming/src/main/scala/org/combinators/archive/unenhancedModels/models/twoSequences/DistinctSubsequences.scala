package org.combinators.archive.unenhancedModels.models.twoSequences

import org.combinators.models._
import org.combinators.models.original.Model

class DistinctSubsequences {
  def instantiate(): Model = {
    val s1 = ArgExpression(0, "s1", StringType(), "r")
    val s2 = ArgExpression(1, "s2", StringType(), "c")

    val boundZero: Expression = ArrayLengthExpression(s1)
    val boundOne: Expression = ArrayLengthExpression(s2)
    val bounds = List(s1, s2) 

    val r: IteratorExpression = IteratorExpression(0, "r")
    val c: IteratorExpression = IteratorExpression(1, "c")

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val DS: Model = new Model(
      "Distinct Subsequences",
      bounds = bounds,
      cases = List(
        (Some(c == zero), one),
        (Some(EqualExpression(CharAtExpression(s1, r - one), CharAtExpression(s2, c - one), CharType())),
          SubproblemExpression(Seq(r - one, c - one)) + SubproblemExpression(Seq(r - one, c))
        ),
        (None, SubproblemExpression(Seq(r - one, c)))
      )
    )

    DS
  }
}
