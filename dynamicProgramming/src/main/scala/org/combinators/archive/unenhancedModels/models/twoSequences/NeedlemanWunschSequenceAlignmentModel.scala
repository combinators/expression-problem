package org.combinators.archive.unenhancedModels.models.twoSequences

import org.combinators.models._

class NeedlemanWunschSequenceAlignmentModel {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0, "s1", StringType(), "r")
    val s2 = new ArgExpression(1, "s2", StringType(), "c")
    val matchBonus = new ArgExpression(2, "matchBonus", IntegerType(), "") // not iterable
    val mismatchPenalty = new ArgExpression(3, "mismatchPenalty", IntegerType(), "") // not iterable
    val gapPenalty = new ArgExpression(4, "gapPenalty", IntegerType(), "") // not iterable

    val boundZero: Expression = new StringLengthExpression(s1)
    val boundOne: Expression = new StringLengthExpression(s2)
    val bounds = List(s1, s2) // boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val score = new TernaryExpression(

      new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
      matchBonus,
      mismatchPenalty
    )

    val NWSA: Model = new Model(
      "PrototypeNWSA",
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
          new MaxExpression(
            new SubproblemExpression(Seq(r - one, c - one)) + score,
            new MaxExpression(
              new SubproblemExpression(Seq(r - one, c)) + gapPenalty,
              new SubproblemExpression(Seq(r, c - one)) + gapPenalty
            )
          )
        )
      )
    )

    NWSA
  }
}
