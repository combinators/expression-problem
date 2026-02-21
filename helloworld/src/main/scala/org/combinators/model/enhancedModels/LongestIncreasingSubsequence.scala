package org.combinators.model.enhancedModels

import org.combinators.model._

class LongestIncreasingSubsequence {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    val arr = new ArgExpression(0, "arr", IntegerArrayType(), "i")
    val bound = List(arr)

    val lenArr = new ArrayLengthExpression(arr)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = HelperExpression("i", one, SelfExpression("i") <= lenArr, lenArr) // only one argument, i

    // what the compute() method calls with helper(1, nums.length-1)
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("j") < i, lenArr) // k will always be within this range

    val sol = SubproblemInvocation(Seq("i"), helpers = Map("i" -> i, "j" -> j))

    val subprobExpr = new SubproblemExpression(Seq(j))
    val checkExpr = new TernaryExpression(new ArrayElementExpression(arr, i) < new ArrayElementExpression(arr, j), subprobExpr + one, zero)

    val innerLoop = new MaxRangeDefinition("j",zero, j<i, checkExpr, j+one)

    val zeroCase = IfThenElseDefinition(i == zero, ExpressionStatement(one), innerLoop)

    val Fib = new EnhancedModel("LIS",
      bound,
      subproblemType = IntegerType(),    // helper methods and intermediate problems are int
      solutionType = StringType(),  // how a solution is represented (not yet effective)
      sol,
      zeroCase,
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new ArrayLengthExpression(arr)))))

    Fib
  }
}
