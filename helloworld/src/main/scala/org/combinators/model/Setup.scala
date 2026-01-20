package org.combinators.model

class Setup {
  def instantiate(): Model = {

    val s1 = new ArgExpression(0)
    val s2 = new ArgExpression(1)

    val boundZero: Expression = new StringLengthExpression(new ArgExpression(0))
    val boundOne: Expression = new StringLengthExpression(new ArgExpression(1))
    val bounds = List(boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0)
    val c: IteratorExpression = new IteratorExpression(1)

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val LCS: Model = new Model("Prototype LCS",
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
          new AdditionExpression(
            new SubproblemExpression(new SubtractionExpression(r, one), new SubtractionExpression(c, one)),
            one
          )
        ),
        (
          None,
          new MaxExpression(
            new SubproblemExpression(r, new SubtractionExpression(c, one)),
            new SubproblemExpression(new SubtractionExpression(r, one), c)
          )
        )
      )
    )

    LCS


    val two: LiteralInt = new LiteralInt(2)

    val bound = List(new ArgExpression(0))

    val i: IteratorExpression = new IteratorExpression(0)
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
          new AdditionExpression(new SubproblemExpression(im1), new SubproblemExpression(im2))
        )
      )
    )
  }
}
