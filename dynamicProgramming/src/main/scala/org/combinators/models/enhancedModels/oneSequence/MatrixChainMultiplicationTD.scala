package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class MatrixChainMultiplicationTD {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = LiteralInt(0)
    val one:  LiteralInt = LiteralInt(1)
    val two:  LiteralInt = LiteralInt(2)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val array = ArgExpression(0, "nums", IntegerArrayType(), "c") 
    val bound = List(array)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= ArrayLengthExpression(array), ArrayLengthExpression(array))   // MOST of this unnecessary
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("i") <= ArrayLengthExpression(array), ArrayLengthExpression(array))   // MOST of this unnecessary

    val k: HelperExpression = HelperExpression("k", i, SelfExpression("k") < j, ArrayLengthExpression(array)) // k will always be within this range

    val helpers = Map("i" -> i, "j" -> j)
    val sol = SubproblemInvocation(Seq("i", "j"), helpers = helpers)   // seq(c,r) is for BOTTOM UP only but i,j are included for TOP DOWN

    /*
     * This is a form of decomposition that applies to upper triangle of the P problem space.
     *
     *   P(i,j) = 0, if i == j
     *   P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
     *      for (int k = i; k < j; k++)
     */
    val subprobExpr = SubproblemExpression(Seq(i, k)) + SubproblemExpression(Seq(k + one, j)) + array(i - one) * array(k) * array(j)

    // Min range definition for k in range from i (inclusive) to j (exclusive) with an advance of k+1
    val defij = MinRangeDefinition("k", i, k < j, subprobExpr, k + one)

    val mcm_definition = IfThenElseDefinition(i == j, ExpressionStatement(zero), defij)

    val MCM = new EnhancedModel("MatrixChainMultiplication",
      bound,
      subproblemType = IntegerType(),  // helper methods and intermediate problems are int
      solutionType   = StringType(),   // how a solution is represented
      sol,
      mcm_definition,
      mode = UpperTriangle(Seq("i", "j")),

      // answer can be found in dp[1][n]
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(one, ArrayLengthExpression(array) - one)))
    )

    MCM
  }
}
