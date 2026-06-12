package org.combinators.archive.unenhancedModels.models.other

import org.combinators.models._
import org.combinators.models.original.Model

class ThreeStringLCSModel {
  def instantiate(): Model = {
    val s1 = ArgExpression(0, "s1", StringType(), "i")
    val s2 = ArgExpression(1, "s2", StringType(), "j")
    val s3 = ArgExpression(2, "s3", StringType(), "k")

    val bounds = List(s1, s2, s3)

    val i: IteratorExpression = IteratorExpression(0, "i")
    val j: IteratorExpression = IteratorExpression(1, "j")
    val k: IteratorExpression = IteratorExpression(2, "k")

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val TSLCS: Model = new Model(
      "ThreeStringLCS",
      bounds = bounds,
      cases = List(
        (Some(i == zero || j == zero || k == zero), zero),
        (
          Some(CharAtExpression(s1, i - one) == CharAtExpression(s2, j - one) && CharAtExpression(s1, i - one) == CharAtExpression(s3, k - one)),
          SubproblemExpression(Seq(i - one, j - one, k - one)) + one
        ),
        (
          None,
          MaxExpression(
            SubproblemExpression(Seq(i - one, j, k)),
            MaxExpression(
              SubproblemExpression(Seq(i, j - one, k)),
              SubproblemExpression(Seq(i, j, k - one))
            )
          )
        )
      )
    )

    TSLCS
  }
}
