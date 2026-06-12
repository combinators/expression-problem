package org.combinators.archive.unenhancedModels.models.twoSequences

import org.combinators.models._
import org.combinators.models.original.Model

@deprecated(message = "Needs to be upgraded to Enhanced Model, since gapPenalty (and others) does not generate properly")
class NeedlemanWunschSequenceAlignmentModel {
  def instantiate(): Model = {
    val s1 = ArgExpression(0, "s1", StringType(), "r")
    val s2 = ArgExpression(1, "s2", StringType(), "c")
    val matchBonus = ArgExpression(2, "matchBonus", IntegerType(), "") // not iterable
    val mismatchPenalty = ArgExpression(3, "mismatchPenalty", IntegerType(), "") // not iterable
    val gapPenalty = ArgExpression(4, "gapPenalty", IntegerType(), "") // not iterable

    val boundZero: Expression = StringLengthExpression(s1)
    val boundOne: Expression = StringLengthExpression(s2)
    val bounds = List(s1, s2) 

    val r: IteratorExpression = IteratorExpression(0, "r")
    val c: IteratorExpression = IteratorExpression(1, "c")

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val score = TernaryExpression(
      CharAtExpression(s1, r - one) == CharAtExpression(s2, c - one),
      matchBonus,
      mismatchPenalty
    )

    val NWSA: Model = new Model(
      "PrototypeNWSA",
      bounds = bounds,
      cases = List(
        (Some(c == zero), r * gapPenalty),
        (Some(r == zero), c * gapPenalty),
        (None,
          MaxExpression(
            SubproblemExpression(Seq(r - one, c - one)) + score,
            MaxExpression(
              SubproblemExpression(Seq(r - one, c)) + gapPenalty,
              SubproblemExpression(Seq(r, c - one)) + gapPenalty
            )
          )
        )
      )
    )

    NWSA
  }
}
