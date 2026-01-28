package org.combinators.model.models.twoSequences

import org.combinators.model._

class UncrossedLinesModel {
  def instantiate(): Model = {
    val nums1 = new ArgExpression(0, "A1", new IntegerArrayType())
    val nums2 = new ArgExpression(1, "A2", new IntegerArrayType())

    val boundZero: Expression = new ArrayLengthExpression(nums1)
    val boundOne: Expression = new ArrayLengthExpression(nums2)
    val bounds = List(nums1, nums2) // boundZero, boundOne)

    val r: IteratorExpression = new IteratorExpression(0, "r")
    val c: IteratorExpression = new IteratorExpression(1, "c")

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val UL: Model = new Model(
      "Uncrossed Lines",
      bounds,
      cases = List(
        (
          Some(r == zero),
          zero
        ),
        (
          Some(c == zero),
          zero
        ),
        (
          Some(new ArrayElementExpression(nums1, r) == new ArrayElementExpression(nums2, c)),
          new AdditionExpression(
            new SubproblemExpression(Seq(r - one, c - one)),
            one
          )
        ),
        (
          None,
          new MaxExpression(
            new SubproblemExpression(Seq(r, c - one)),
            new SubproblemExpression(Seq(r - one, c))
          )
        )
      )
    )

    UL
  }
}
