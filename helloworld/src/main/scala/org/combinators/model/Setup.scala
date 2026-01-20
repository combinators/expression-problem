package org.combinators.model

class Setup {
  def instantiate(): Model = {


    val boundZero: Expression =new StringLengthExpression(new ArgExpression(0))
    val boundOne: Expression =new StringLengthExpression(new ArgExpression(1))
    val bounds = List(boundZero,boundOne)

    val r: IteratorExpression = new IteratorExpression(0)
    val c: IteratorExpression = new IteratorExpression(1)
    val iterators = List(r, c)

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val LCS: Model = new Model("Prototype",
      bounds,
      cases = List(
        (
          Some(new EqualExpression(r, zero)),
          zero
        ),
        (
          Some(new EqualExpression(c, zero)),
          zero
        ),
        (
          Some(new EqualExpression(new CharAtExpression(s1, r), new CharAtExpression(s2, c))),
          Some()
        ),
        (
          None,
          new Expression()
        )
      )
    )

    LCS


    val Fib: Model = new Model("Fibonacci",
      n,
      iterators,
      cases = List(
        (
          Some(new EqualExpression(r, zero)),
          zero
        ),
        (
          Some(new EqualExpression(c, zero)),
          zero
        ),
        (
          Some(new EqualExpression(new CharAtExpression(s1, r), new CharAtExpression(s2, c))),
          new IntegerExpression
        ),
        (
          None,
          new IntegerExpression()
        )
      )
    )
  }
}
