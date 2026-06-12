package org.combinators.models.enhancedModels.integer

import org.combinators.models._

class Fibonacci {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val two: LiteralInt = LiteralInt(2)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val n = ArgExpression(0, "n", IntegerType(), "i")
    val bound = List(n)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= n, n + one) // only one argument, i

    // what the compute() method calls with helper(1, nums.length-1)
    val sol = SubproblemInvocation(Seq("i"), helpers = Map("i" -> i))

    val oneCase = IfThenElseDefinition(i == one, ExpressionStatement(one),
      ExpressionDefinition(SubproblemExpression(Seq(i - one)) + SubproblemExpression(Seq(i - two))))

    val zeroCase = IfThenElseDefinition(i == zero, ExpressionStatement(zero), oneCase)

    val Fib = new EnhancedModel("Fibonacci",
      bound,
      subproblemType = IntegerType(),    // helper methods and intermediate problems are int
      solutionType = StringType(),  // how a solution is represented (not yet effective)
      sol,
      zeroCase,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(n))))

    Fib
  }
}
