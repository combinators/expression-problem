package org.combinators.model.models.grid

import org.combinators.model._

class UniquePathsModel {
  def instantiate(): Model = {
    val m = new ArgExpression(0, "m", new IntegerType(), "r")
    val n = new ArgExpression(1, "n", new IntegerType(), "c")
    val bounds = List(m, n)

    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)

    val UP: Model = new Model(
      "Unique Paths",
      bounds = bounds,
      cases = List(
        (
          Some(n == zero || m == zero),
          one
        ),
        (
          None,
          new SubproblemExpression(Seq(m - one, n)) + new SubproblemExpression(Seq(m, n - one))
        )
      )
    )

    UP
  }
}
