package org.combinators.model.models.twoSequences

import org.combinators.model._

class LongestCommonSubsequenceModel {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0)
    val s2 = new ArgExpression(1)

    val boundZero: Expression = new StringLengthExpression(new ArgExpression(0))
    val boundOne: Expression = new StringLengthExpression(new ArgExpression(1))
    val bounds = List(boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val subp1 : Seq[Expression] = Seq(
      new SubtractionExpression(r, one), new SubtractionExpression(c, one)
    )

    val subp2 : Seq[Expression] = Seq(
      r, new SubtractionExpression(c, one)
    )

    val subp3 : Seq[Expression] = Seq(
      new SubtractionExpression(r, one), c
    )

    val LCS: Model = new Model("Prototype LCS",
      bounds,
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
          Some(new EqualExpression(new CharAtExpression(s1, r), new CharAtExpression(s2, c))),
          new AdditionExpression(
            new SubproblemExpression(subp1),
            one
          )
        ),
        (
          None,
          new MaxExpression(
            new SubproblemExpression(subp2),
            new SubproblemExpression(subp3)
          )
        )
      )
    )

    LCS
  }
}
