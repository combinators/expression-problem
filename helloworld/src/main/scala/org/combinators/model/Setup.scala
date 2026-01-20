package org.combinators.model

class Setup {
  def instantiate(): Model = {
    val s1: StringExpression = new StringExpression()
    val s2: StringExpression = new StringExpression()
    val inputs = List(s1, s2)

    val r: IteratorExpression = new IteratorExpression(0)
    val c: IteratorExpression = new IteratorExpression(1)
    val iterators = List(r, c)

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val LCS: Model = new Model("Prototype",
      inputs,
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

    LCS
  }
}
