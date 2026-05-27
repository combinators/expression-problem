package org.combinators.archive.unenhancedModels.models.oneSequence

import org.combinators.models._
import org.combinators.models.original.Model

class FibonacciModel {
  def instantiate(): Model = {

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val two: LiteralInt = LiteralInt(2)

    val bound = List(ArgExpression(0, "n", IntegerType(), "i"))

    val i: IteratorExpression = IteratorExpression(0, "i")

    val Fib: Model = new Model("Fibonacci",
      bound,
      cases = List(
        (Some(i == zero),    zero),
        (Some(i == one),     one),
        (None,               SubproblemExpression(Seq(i - one)) + SubproblemExpression(Seq(i - two)))
      )
    )

    Fib

  }
}