package org.combinators.model.models.oneSequence

import org.combinators.model._

class MaxSubarrayModel {
  def instantiate(): Model = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val bounds = List(new ArrayLengthExpression(new ArgExpression(0,"nums",new IntegerType)), 2)
    val fakeBound = List(new ArgExpression(0,"nums",new IntegerType))

    val i: IteratorExpression = new IteratorExpression(0, "i")
    val m: IteratorExpression = new IteratorExpression(0, "m")

    val cur= new ArrayElementExpression(new ArgExpression(0,"nums",new IntegerType), i)


    val MaxSubarray: Model = new Model("MaxSubarray",
      fakeBound,
      cases = List(
        (
          Some( new EqualExpression(i, zero)),
          cur
        ),
        (
          Some(new EqualExpression(m,zero)),
          new MaxExpression(new SubproblemExpression(Seq(i-one,zero))+cur, cur)
        ),
        (
          None,
          new MaxExpression(new SubproblemExpression(Seq(i,zero)),new SubproblemExpression(Seq(i-one,one)))
        )
      )
    )

    MaxSubarray

  }
}