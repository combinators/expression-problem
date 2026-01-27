package org.combinators.model.models.twoSequences

import org.combinators.model._

class NeedlemanWunschSequenceAlignmentModel {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0)
    val s2 = new ArgExpression(1)
    val matchBonus = new ArgExpression(2)
    val mismatchPenalty = new ArgExpression(3)
    val gapPenalty = new ArgExpression(4)

    val boundZero: Expression = new StringLengthExpression(s1)
    val boundOne: Expression = new StringLengthExpression(s2)
    val bounds = List(boundZero, boundOne)

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
          one // todo: implement
        )
      )
    )

    NWSA
  }
}
