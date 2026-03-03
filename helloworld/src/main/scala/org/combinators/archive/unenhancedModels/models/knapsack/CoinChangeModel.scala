package org.combinators.archive.unenhancedModels.models.knapsack

import org.combinators.models._

class CoinChangeModel {
  def instantiate(): Model = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val arrayArg = new ArgExpression(0, "coins", new IntegerArrayType(), "c")
    val amount = new ArgExpression(1, "amount", new IntegerType(), "a")

    val c: IteratorExpression = new IteratorExpression(0, "c")
    val a: IteratorExpression = new IteratorExpression(1, "a")

    val coinscm1 = new ArrayElementExpression(arrayArg,c-one)


    val Knapsack: Model = new Model("CoinChange",
      List(arrayArg,amount),
      cases = List(
        (
          Some(new EqualExpression(a, zero)),
          zero
        ),
        (
          Some(new EqualExpression(c, zero)),
          new LiteralInt(1073741823)
        ),
        (
          Some(new LessThanExpression(amount, coinscm1)),
          new SubproblemExpression(Seq(c-one))
        ),

        (
          None,
           new MinExpression(
             new SubproblemExpression(Seq(c, amount-coinscm1))+one,
             new SubproblemExpression(Seq(c-one, amount)))
        )
      )
    )

    Knapsack

  }
}