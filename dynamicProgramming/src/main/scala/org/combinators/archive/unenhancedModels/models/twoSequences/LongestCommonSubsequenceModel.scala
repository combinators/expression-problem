package org.combinators.archive.unenhancedModels.models.twoSequences

import org.combinators.models._
import org.combinators.models.original.Model

class LongestCommonSubsequenceModel {
  def instantiate(): Model = {
    val s1 = ArgExpression(0, "s1", StringType(), "r")
    val s2 = ArgExpression(1, "s2", StringType(), "c")

    val boundZero: Expression = StringLengthExpression(s1)
    val boundOne: Expression = StringLengthExpression(s2)
    val bounds = List(s1, s2) 

    val r: IteratorExpression = IteratorExpression(0, "r")
    val c: IteratorExpression = IteratorExpression(1, "c")

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val LCS: Model = new Model("PrototypeLCS",   // cannot have space in the name since this becomes a file
      bounds = bounds,
      cases = List(
        (Some(r == zero), zero),
        (Some(c == zero), zero),
        (Some(CharAtExpression(s1, r-one) == CharAtExpression(s2, c-one)),
         SubproblemExpression(Seq(r - one, c - one)) + one  
        ),
        (
          None,
          MaxExpression(
            SubproblemExpression(Seq(SubtractionExpression(r, one), c)),
            SubproblemExpression(Seq(r, c - one))
          )
        )
      )
    )

    LCS
  }
}
