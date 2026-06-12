package org.combinators.models.enhancedModels.integer

import org.combinators.models._

class DiceThrow {
  /**
   * Name: Dice Throw
   * Description:
   * Given n dices each with m faces, numbered from 1 to m, find the number of ways to get sum x,
   * which is the summation of values on each face
   *
   * Example:
   * m=2, n=3, x=6
   * there is only 1 way to get the sum 6 using 3 dices from 1 to 2
   */

  def model: EnhancedModel = {
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)

    val m = ArgExpression(2, "m", IntegerType(), "m")    // m never changes and is not part of helper
    val n = ArgExpression(0, "n", IntegerType(), "i")    // not sure if 'i' is used
    val x = ArgExpression(1, "x", IntegerType(), "j")

    val bound_ps = List(m, n, x)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= n, n + one)   // need to be zero for BottomUp to be able to trigger base case within loop.
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("j") <= x, x + one)

    val k: HelperExpression = HelperExpression("k", i, SelfExpression("k") <= m, m)            // in_range is not essential since this is not an argument to helper/subproblem

    val additiveExpression:Expression = SubproblemExpression(Seq(i - one, j - k))
    val k_sum = SumDefinition("k", one, k <= m && zero <= j - k, additiveExpression, k + one)

    val helperTable = Map("i" -> i, "j" -> j, "k" -> k)
    val sol_dt = SubproblemInvocation(Seq("i", "j"), helpers = helperTable)

    val base2 = IfThenElseDefinition(i == zero || j <= zero, ExpressionStatement(zero), k_sum)
    val dt_definition = IfThenElseDefinition(i == zero && j == zero, ExpressionStatement(one), base2)

    val DiceThrow = new EnhancedModel("DiceThrow",
      bound_ps,
      subproblemType = IntegerType(),    // helper method is an int
      solutionType   = StringType(),     // solution is a string
      sol_dt,
      dt_definition,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(n, x)))
    )

    DiceThrow
  }
}
