package org.combinators.archive.unenhancedModels.models.other

import org.combinators.models._

class ThreeStringLCSModel {
  def instantiate(): Model = {
    val s1 = new ArgExpression(0, "s1", StringType(), "i")
    val s2 = new ArgExpression(1, "s2", StringType(), "j")
    val s3 = new ArgExpression(2, "s3", StringType(), "k")

    val bounds = List(s1, s2, s3)

    val i: IteratorExpression = new IteratorExpression(0, "i")
    val j: IteratorExpression = new IteratorExpression(1, "j")
    val k: IteratorExpression = new IteratorExpression(2, "k")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val TSLCS: Model = new Model(
      "ThreeStringLCS",
      bounds = bounds,
      cases = List(
        (
          Some(
            new EqualExpression(i, zero) ||
            new EqualExpression(j, zero) ||
            new EqualExpression(k, zero)
          ),
          zero
        ),
        (
          Some(new CharAtExpression(s1, i - one) == new CharAtExpression(s2, j - one) && new CharAtExpression(s1, i - one) == new CharAtExpression(s3, k - one)),
          new SubproblemExpression(Seq(i - one, j - one, k - one)) + one
        ),
        (
          None,
          new MaxExpression(
            new SubproblemExpression(Seq(i - one, j, k)),
            new MaxExpression(
              new SubproblemExpression(Seq(i, j - one, k)),
              new SubproblemExpression(Seq(i, j, k - one))
            )
          )
        )
      )
    )

    TSLCS
  }
}
