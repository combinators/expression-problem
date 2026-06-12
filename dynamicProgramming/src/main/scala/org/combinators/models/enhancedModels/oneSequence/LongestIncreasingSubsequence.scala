package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class LongestIncreasingSubsequence {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val two: LiteralInt = LiteralInt(2)

    val arr = ArgExpression(0, "arr", IntegerArrayType(), "i")
    val bound = List(arr)

    val lenArr = ArrayLengthExpression(arr)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = HelperExpression("i", one, SelfExpression("i") <= lenArr, lenArr) // only one argument, i

    // what the compute() method calls with helper(1, nums.length-1)
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("j") < i, lenArr) // k will always be within this range

    val sol = SubproblemInvocation(Seq("i"), helpers = Map("i" -> i, "j" -> j))

    val subprobExpr = SubproblemExpression(Seq(j))
    val checkExpr = TernaryExpression(ArrayElementExpression(arr, i) < ArrayElementExpression(arr, j), subprobExpr + one, zero)

    val innerLoop = MaxRangeDefinition("j",zero, j < i, checkExpr, j+one)

    val zeroCase = IfThenElseDefinition(i == zero, ExpressionStatement(one), innerLoop)

    val Fib = new EnhancedModel("LIS",
      bound,
      subproblemType = IntegerType(),    // helper methods and intermediate problems are int
      solutionType = StringType(),  // how a solution is represented (not yet effective)
      sol,
      zeroCase,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(ArrayLengthExpression(arr)))))

    Fib
  }
}
