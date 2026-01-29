package org.combinators.model

class Setup {
  def instantiate(): Model = {

    val s1 = new ArgExpression(0, "s1", new StringType(), "r")
    val s2 = new ArgExpression(1, "s2", new StringType(), "c")

    val boundZero: Expression = new StringLengthExpression(s1)
    val boundOne: Expression = new StringLengthExpression(s2)
    val bounds = List(s1, s2)  // was bounds but that's not right

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val subp1 : Seq[Expression] = Seq(
      new SubtractionExpression(r, one), new SubtractionExpression(c, one)
    )

    val subp2 : Seq[Expression] = Seq(
      r, new SubtractionExpression(c, one)
    )

    val subp3 : Seq[Expression] = Seq(
      new SubtractionExpression(r, one), c
    )

    val LCS: Model = new Model("PrototypeLCS",   // cannot contain a space since it represents a class
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
            new SubproblemExpression(subp1),
            one
          )
        ),
        (
          None,
          new MaxExpression(
            new SubproblemExpression(subp2),
            new SubproblemExpression(subp3)
          )
        )
      )
    )

    val two: LiteralInt = new LiteralInt(2)

    val bound = List(new ArgExpression(0, "n", new IntegerType(), "i"))

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
          new AdditionExpression(new SubproblemExpression(Seq(im1)), new SubproblemExpression(Seq(im2)))
        )
      )
    )

    Fib
  }
}
