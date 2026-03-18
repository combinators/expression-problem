package org.combinators.archive.unenhancedModels.models.knapsack

import org.combinators.models._

class KnapsackModel {
  def instantiate(): Model = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    //the array should be 2d
    val arrayArg = new ArgExpression(0, "arr", new IntegerArray2DType(), "i")
    val weight = new ArgExpression(1, "W", new IntegerType(), "w")

    val i: IteratorExpression = new IteratorExpression(0, "i")
    val w: IteratorExpression = new IteratorExpression(1, "w")

    val weightim1= new ArrayElementExpression(new ArrayElementExpression(arrayArg,i-one),zero)
    val valueim1= new ArrayElementExpression(new ArrayElementExpression(arrayArg,i-one),one)


    val Knapsack: Model = new Model("Knapsack",
      List(arrayArg,weight),
      cases = List(
        (
          Some(new EqualExpression(i, zero)),
          zero
        ),
        (
          Some(new EqualExpression(w, zero)),
          zero
        ),

        (
          None,
           new MaxExpression(
             new SubproblemExpression(List(i-one,w)),
             new TernaryExpression(
               weightim1<w,
               valueim1 + new SubproblemExpression(List(i-one,w-weightim1)),
               zero)
             )
        )
      )
    )

    Knapsack

  }
}