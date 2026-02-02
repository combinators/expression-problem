package org.combinators.model.models.twoSequences

import org.combinators.model._

class NeedlemanWunschSequenceAlignmentModel {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0, "s1", new StringType(), "r")
    val s2 = new ArgExpression(1, "s2", new StringType(), "c")
    val matchBonus = new ArgExpression(2, "matchBonus", new IntegerType(), "")            // not iterable
    val mismatchPenalty = new ArgExpression(3, "mismatchPenalty", new IntegerType(), "")  // not iterable
    val gapPenalty = new ArgExpression(4, "gapPenalty", new IntegerType(), "")            // not iterable

    val boundZero: Expression = new StringLengthExpression(s1)
    val boundOne: Expression = new StringLengthExpression(s2)
    val bounds = List(s1, s2) // boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val NWSA: Model = new Model(
      "Needleman-Wunsch Sequence Alignment",
      bounds = bounds,
      cases = List(
        (
          Some(c == zero),
          r * gapPenalty
        ),
        (
          Some(r == zero),
          c * gapPenalty
        ),
        (
          None,
          {
            val score = new TernaryExpression(
              new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
              matchBonus,
              mismatchPenalty
            )

            new MaxExpression(
              new SubproblemExpression(Seq(r - one, c - one)) + score,
              new MaxExpression(
                new SubproblemExpression(Seq(r - one, c)),
                new SubproblemExpression(Seq(r, c - one))
              )
            )
          }
        )
      )
    )

    NWSA
  }
}
