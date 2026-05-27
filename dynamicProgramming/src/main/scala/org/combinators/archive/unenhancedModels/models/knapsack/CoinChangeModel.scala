package org.combinators.archive.unenhancedModels.models.knapsack

import org.combinators.models._
import org.combinators.models.original.Model

class CoinChangeModel {
  def instantiate(): Model = {

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val arrayArg = ArgExpression(0, "coins", IntegerArrayType(), "c")
    val amount = ArgExpression(1, "amount", IntegerType(), "a")

    val c: IteratorExpression = IteratorExpression(0, "c")
    val a: IteratorExpression = IteratorExpression(1, "a")

    val coinscm1 = ArrayElementExpression(arrayArg,c-one)


    val Knapsack: Model = new Model("CoinChange",
      List(arrayArg,amount),
      cases = List(
        (Some(a == zero), zero),
        (Some(c == zero), LiteralInt(1073741823)),
        (Some(amount < coinscm1),
          SubproblemExpression(Seq(c-one))
        ),
        (
          None,
           MinExpression(
             SubproblemExpression(Seq(c, amount - coinscm1)) + one,
             SubproblemExpression(Seq(c - one, amount)))
        )
      )
    )

    Knapsack

  }
}