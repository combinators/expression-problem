package org.combinators.model.models.twoSequences

import org.combinators.model._

class LongestCommonSubsequenceModel {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0, "s1", new StringType(), "r")
    val s2 = new ArgExpression(1, "s2", new StringType(), "c")

    val boundZero: Expression = new StringLengthExpression(s1)
    val boundOne: Expression = new StringLengthExpression(s2)
    val bounds = List(s1, s2) // boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val LCS: Model = new Model("PrototypeLCS",   // cannot have space in the name since this becomes a file
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
          Some(new EqualExpression(new CharAtExpression(s1, r-one), new CharAtExpression(s2, c-one), new CharType())),
          new AdditionExpression(new SubproblemExpression(Seq(r - one, c - one)), one)            // how does "-" work here? IT SHOULDN'T
        ),
        (
          None,
          new MaxExpression(
            new SubproblemExpression(Seq(new SubtractionExpression(r, one), c)),
            new SubproblemExpression(Seq(r, new SubtractionExpression(c, one)))
          )
        )
      )
    )

    LCS
  }
}
