package org.combinators.model.models.oneSequence

import org.combinators.model._

class FibonacciModel {
  def instantiate(): Model = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    val bound = List(new ArgExpression(0))

    val i: IteratorExpression = new IteratorExpression(0, "i")
    val im1 = new SubtractionExpression(i, one)
    val im2 = new SubtractionExpression(i, two)




    val Fib: Model = new Model("Fibonacci",
      bound,
      cases = List(
        (
          Some(new EqualExpression(i, zero)),
          zero
        ),
        (
          Some(new EqualExpression(i, one)),
          one
        ),
        (
          None,
           new SubproblemExpression(Seq(im1)) + new SubproblemExpression(Seq(im2))
        )
      )
    )

    Fib

  }
}