package org.combinators.model.models.knapsack

import org.combinators.model._

class KnapsackModel {
  def instantiate(): Model = {

    val negone: LiteralInt = new LiteralInt(-1)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    //the array should be 2d
    val arrayArg = new ArgExpression(0, "arr", new IntegerArrayType(), "i")
    val weight = new ArgExpression(1, "W", new IntegerType(), "w")

    val i: IteratorExpression = new IteratorExpression(0, "i")
    val w: IteratorExpression = new IteratorExpression(1, "w")

    val weighti= new ArrayElementExpression(new ArrayElementExpression(arrayArg,i),zero)
    val valuei= new ArrayElementExpression(new ArrayElementExpression(arrayArg,i),one)



    val Knapsack: Model = new Model("Knapsack",
      List(arrayArg,weight),
      cases = List(
        (
          Some(new EqualExpression(i, negone)),
          zero
        ),
        (
          Some(new EqualExpression(w, zero)),
          zero
        ),

//        Math.max(
//          helper(i-1,W),
//          weights[i]<=W?
        //          (values[i]+helper(i-1,W-weights[i])):
        //          0
//        )
        (
          None,
           new MaxExpression(
             new SubproblemExpression(List(i-one,w)),
             new TernaryExpression(
               weighti<w,
               valuei+new SubproblemExpression(List(i-one,w-weighti)),
               zero)
             )
        )
      )
    )

    Knapsack

  }
}