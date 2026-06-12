package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class MatrixChainMultiplicationBU {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = LiteralInt(0)
    val one:  LiteralInt = LiteralInt(1)
    val two:  LiteralInt = LiteralInt(2)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val array = ArgExpression(0, "nums", IntegerArrayType(), "c")
    val bound = List(array)

    // Need to find way to get these (i,j) into the EnhancedModel. Apply the mapping that iteration takes place over (r,c) and there is
    // mapping of i = r+c+2 and j = c+1. The inherent problem search is upper triangle matrix of the P(i,j) space, which turns out to
    // be upper left triangular matrix over (r,c)
    val c: HelperExpression = HelperExpression("c", two, SelfExpression("c") <= ArrayLengthExpression(array) - one, ArrayLengthExpression(array) + one)
    val r: HelperExpression = HelperExpression("r", one, SelfExpression("r") <= ArrayLengthExpression(array) - c, ArrayLengthExpression(array) + one)

    val k: HelperExpression = HelperExpression("k", r, SelfExpression("k") < (r + c - one), ArrayLengthExpression(array)) // k will always be within this range

    val helpers = Map("c" -> c, "r" -> r)
    val mappers = Map("c" -> r, "r" -> (r + c - one))   // controls how problems are "mapped" into the dp[i][j] design space
    val sol = SubproblemInvocation(Seq("c", "r"), helpers = helpers, mappers = mappers)

    /*
     * This is a form of decomposition that applies to upper triangle of the P problem space.
     *
     *   P(i,j) = 0, if i == j
     *   P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
     *      for (int k = i; k < j; k++)
     */
    val subprobExpr = SubproblemExpression(Seq(r, k)) + SubproblemExpression(Seq(k + one, r + c - one)) + array(r - one) * array(k) * array(r + c - one)

    // Min range definition for k in range from i (inclusive) to j (exclusive) with an advance of k+1
    val defij = MinRangeDefinition("k", r, k < r + c - one, subprobExpr, k + one)

    val mcm_definition = IfThenElseDefinition(r == r + c - one, ExpressionStatement(zero), defij)

    val MCM = new EnhancedModel("MatrixChainMultiplication",
      bound,
      subproblemType = IntegerType(),  // helper methods and intermediate problems are int
      solutionType   = StringType(),   // how a solution is represented
      sol,
      mcm_definition,
      mode = UpperTriangle(Seq("c", "r")),

      // answer can be found in dp[1][n]
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(one, ArrayLengthExpression(array) - one)))
    )

    MCM
  }
}
