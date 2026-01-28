package org.combinators.model.models.twoSequences

import org.combinators.model._

class DistinctSubsequences {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0, "s1", new StringType())
    val s2 = new ArgExpression(1, "s2", new StringType())

    val boundZero: Expression = new ArrayLengthExpression(s1)
    val boundOne: Expression = new ArrayLengthExpression(s2)
    val bounds = List(s1, s2) // was boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val DS: Model = new Model(
      "Distinct Subsequences",
      bounds = bounds,
      cases = List(
        (
          Some(c == zero),
          one
        ),
        (
          Some(new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one)),
          new SubproblemExpression(Seq(r - one, c - one)) + new SubproblemExpression(Seq(r - one, c))
        ),
        (
          None,
          new SubproblemExpression(Seq(r - one, c))
        )
      )
    )

    DS
  }
}
