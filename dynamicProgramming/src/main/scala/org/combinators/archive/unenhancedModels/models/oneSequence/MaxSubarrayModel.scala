package org.combinators.archive.unenhancedModels.models.oneSequence

import org.combinators.models._
import org.combinators.models.original.Model

class MaxSubarrayModel {
  def instantiate(): Model = {

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val bounds = List(ArrayLengthExpression(ArgExpression(0, "nums", IntegerType(), "i") ), 2)
    val fakeBound = List(ArgExpression(0, "nums", IntegerType(), "m"))    // not sure if these are right.

    val i: IteratorExpression = IteratorExpression(0, "i")
    val m: IteratorExpression = IteratorExpression(0, "m")

    val cur = ArrayElementExpression(ArgExpression(0, "nums", IntegerType(), "m"), i)

    val MaxSubarray: Model = new Model("MaxSubarray",
      fakeBound,
      cases = List(
        (Some(i == zero), cur),
        (Some(m == zero), MaxExpression(SubproblemExpression(Seq(i - one, zero)) + cur, cur)),
        (None, MaxExpression(SubproblemExpression(Seq(i, zero)), SubproblemExpression(Seq(i - one, one))))
      )
    )

    MaxSubarray

  }
}