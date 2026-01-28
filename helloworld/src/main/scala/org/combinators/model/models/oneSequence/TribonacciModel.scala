package org.combinators.model.models.oneSequence

import org.combinators.model._

class TribonacciModel {
  def instantiate(): Model = {
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)
    val three: LiteralInt = new LiteralInt(3)

    val bound = List(new ArgExpression(0, "n", new IntegerType()))

    val i: IteratorExpression = new IteratorExpression(0, "i")

    val im1 = new SubtractionExpression(i, one)
    val im2 = new SubtractionExpression(i, two)
    val im3 = new SubtractionExpression(i, three)

    val Tribonacci: Model = new Model("Tribonacci",
      bounds = bound,
      cases = List(
        (
          Some(new EqualExpression(i, zero)),
          zero
        ),
        (
          Some(new orExpression(new EqualExpression(i, one), new EqualExpression(i, two))),
          one
        ),
        (
          None,
          im1 + im2 + im3
        )
      )
    )

    Tribonacci
  }
}
