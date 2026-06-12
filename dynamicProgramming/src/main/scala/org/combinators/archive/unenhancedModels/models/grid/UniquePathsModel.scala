package org.combinators.archive.unenhancedModels.models.grid

import org.combinators.models._
import org.combinators.models.original.Model

class UniquePathsModel {
  def instantiate(): Model = {
    val m = ArgExpression(0, "m", IntegerType(), "r")
    val n = ArgExpression(1, "n", IntegerType(), "c")
    val bounds = List(m, n)

    val zero = LiteralInt(0)
    val one = LiteralInt(1)

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
          SubproblemExpression(Seq(m - one, n)) + SubproblemExpression(Seq(m, n - one))
        )
      )
    )

    UP
  }
}
