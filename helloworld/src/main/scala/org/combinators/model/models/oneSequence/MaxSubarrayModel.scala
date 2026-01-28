package org.combinators.model.models.oneSequence

import org.combinators.model._

class MaxSubarrayModel {
  def instantiate(): Model = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    val bounds = List(new ArrayLengthExpression(new ArgExpression(0)))

    val i: IteratorExpression = new IteratorExpression(0, "i")
    val m: IteratorExpression = new IteratorExpression(0, "m")

    val cur= new ArrayElementExpression(new ArgExpression(0), i)


    val MaxSubarray: Model = new Model("MaxSubarray",
      bounds,
      cases = List(
        (
          Some(new EqualExpression(i, zero)),
          cur
        ),
        (
          None,
          new MaxExpression(new SubproblemExpression(Seq(i-one))+cur, cur)
        )
      )
    )

    MaxSubarray

  }
}