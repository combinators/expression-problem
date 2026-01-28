package org.combinators.model.models.twoSequences

import org.combinators.model._

class LongestCommonSubsequenceModel {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0, "s1", new StringType())
    val s2 = new ArgExpression(1, "s2", new StringType())

    val boundZero: Expression = new StringLengthExpression(s1)
    val boundOne: Expression = new StringLengthExpression(s2)
    val bounds = List(s1, s2) // boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val LCS: Model = new Model("Prototype LCS",
      bounds = bounds,
      cases = List(
        (
          Some(new EqualExpression(r, zero)),
          zero
        ),
        (
          Some(new EqualExpression(c, zero)),
          zero
        ),
        (
          Some(new CharAtExpression(s1, r) == new CharAtExpression(s2, c)),
          new SubproblemExpression(Seq(r - one, c - one)) + one
        ),
        (
          None,
          new MaxExpression(
            new SubproblemExpression(Seq(r - one, c)),
            new SubproblemExpression(Seq(r, c - one))
          )
        )
      )
    )

    LCS
  }
}
