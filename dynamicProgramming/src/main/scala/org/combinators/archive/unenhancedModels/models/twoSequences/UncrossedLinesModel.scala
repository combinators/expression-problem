package org.combinators.archive.unenhancedModels.models.twoSequences

import org.combinators.models._
import org.combinators.models.original.Model

class UncrossedLinesModel {
  def instantiate(): Model = {
    val nums1 = ArgExpression(0, "A1", IntegerArrayType(), "r")
    val nums2 = ArgExpression(1, "A2", IntegerArrayType(), "c")

    val boundZero: Expression = ArrayLengthExpression(nums1)
    val boundOne: Expression = ArrayLengthExpression(nums2)
    val bounds = List(nums1, nums2) 

    val r: IteratorExpression = IteratorExpression(0, "r")
    val c: IteratorExpression = IteratorExpression(1, "c")

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val UL: Model = new Model(
      "PrototypeUL",
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
          Some(ArrayElementExpression(nums1, SubtractionExpression(r, one)) == ArrayElementExpression(nums2, SubtractionExpression(c, one))),
          AdditionExpression(
            SubproblemExpression(Seq(r - one, c - one)),
            one
          )
        ),
        (
          None,
          MaxExpression(
            SubproblemExpression(Seq(r, c - one)),
            SubproblemExpression(Seq(r - one, c))
          )
        ),
      )
    )

    UL
  }
}
