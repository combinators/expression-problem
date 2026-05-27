package org.combinators.models.enhancedModels.integer

import org.combinators.models._

/**
 * Name: Bell Number
 * Description:
 * Given a set of n elements, find the number of ways of partitioning it
 *
 * Example:
 * n=2
 * number of ways = 2, since set {1,2} can be partition as {{1}, {2}} and {1,2}
 */
class BellNumber {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val n = ArgExpression(0, "n", IntegerType(), "i")
    val bound = List(n)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= n, n + one)            // in_range is not essential since this is not an argument to helper/subproblem
    val k: HelperExpression = HelperExpression("k", zero, SelfExpression("k") <= n, n + one)            // in_range is not essential since this is not an argument to helper/subproblem

    val helperTable = Map( "i" -> i, "k" -> k )
    val sol = SubproblemInvocation(Seq("i", "k"), helpers = helperTable)

    val expr = k * SubproblemExpression(Seq(i - one, k)) + SubproblemExpression(Seq(i - one, k - one))

    // Parition of i elements by k is ONE, whenever i==k (like C (n,n)) or i==1 (like C(n,1)).
    val base3 = IfThenElseDefinition(i == k || k == one, ExpressionStatement(one), ExpressionDefinition(expr))
    val base2 = IfThenElseDefinition(i == zero || k == zero, ExpressionStatement(zero), base3)
    val dt_definition = IfThenElseDefinition(i == zero && k == zero, ExpressionStatement(one), base2)

    // Top-Down would be OK with SumDefinition,  but we cannot use this for BottomUp, which sets individual
    // DP[] entries based on the Sum. Instead, we need a new
    val final_answer = ReturnAccumulatedDefinition(
      "sum",
      Seq(AccumulatorInformation("idx", zero, SelfExpression("idx") <= n, SelfExpression("idx") + one)),
      SubproblemExpression(Seq(n, SelfExpression("idx")))
    )

    val BellNumber = new EnhancedModel("BellNumber",
      bound,
      subproblemType = IntegerType(),    // helper methods and intermediate problems are int
      solutionType = StringType(),  // how a solution is represented (not yet effective)
      sol,
      dt_definition,
      answer = final_answer
    )

    BellNumber
  }
}
