package org.combinators.archive.unenhancedModels.models.knapsack

import org.combinators.models._
import org.combinators.models.original.Model

class KnapsackModel {
  def instantiate(): Model = {

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    //the array should be 2d
    val arrayArg = ArgExpression(0, "arr", IntegerArray2DType(), "i")
    val weight = ArgExpression(1, "W", IntegerType(), "w")

    val i: IteratorExpression = IteratorExpression(0, "i")
    val w: IteratorExpression = IteratorExpression(1, "w")

    val weightim1= ArrayElementExpression(ArrayElementExpression(arrayArg, i - one), zero)
    val valueim1= ArrayElementExpression(ArrayElementExpression(arrayArg, i - one), one)


    val Knapsack: Model = new Model("Knapsack",
      List(arrayArg,weight),
      cases = List(
        (Some(i == zero), zero),
        (Some(w == zero), zero),
        (
          None,
           MaxExpression(
             SubproblemExpression(List(i - one, w)),
             TernaryExpression(
               weightim1 < w,
               valueim1 + SubproblemExpression(List(i - one, w - weightim1)),
               zero)
             )
        )
      )
    )

    Knapsack

  }
}