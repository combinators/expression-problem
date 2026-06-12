package org.combinators.models.enhancedModels.grid

import org.combinators.models.*

/**
 * Name: Number Paths With Exactly K Coins
 * Description:
 * Given a matrix mat[][] where each cell contains a certain number of coins, and an integer k,
 * count the total number of distinct paths from the top-left cell to the bottom-right cell such 
 * that the sum of coins collected along the path is exactly k.
 * 
 * URL: https://www.geeksforgeeks.org/dsa/number-of-paths-with-exactly-k-coins/
 * 
 * Recurrence:
 * P(r, 0) = 1 only one way to reach any cell in the first column (go straight down)
 * P(0, c) = 1 only one way to reach any cell in the first row (go straight right)
 * P(r, c) = P(r-1, c) + P(r, c-1)
 */
@deprecated(message = "NOT WORKING! NEEDS multiple statements in BODY and model cannot support this yet")
class NumberPathsWithKCoins {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)

    val grid = ArgExpression(0, "grid", IntegerArray2DType(), "g")
    val kParam = ArgExpression(1, "k", IntegerType(), "k")

    val m = ArrayLengthExpression(grid)
    val n = ArrayLengthExpression(ArrayElementExpression(grid, zero))
    val bound = List(grid, kParam)

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") < m, m)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") < n, n)
    val k: HelperExpression = HelperExpression("k", zero, SelfExpression("k") < kParam, kParam + one)

    val helpers = Map("r" -> r, "c" -> c, "k" -> k)
    val sol = SubproblemInvocation(order = Seq("r", "c", "k"), helpers = helpers, returnType = IntegerType())

    val gridRC = ArrayElementExpression(ArrayElementExpression(grid, r), c)
    val enoughCoins = k - gridRC >= zero

    // if not exceeded total, then P(r, c) = P(r-1, c) + P(r, c-1), otherwise must equal 0
    val subproblemTraversal = IfThenElseDefinition(AndExpression(enoughCoins, r > zero),
      ExpressionStatement(ArrayElementExpression(ArrayElementExpression(grid, r), c) + SubproblemExpression(Seq(r - one, c, k - gridRC))),
      ExpressionDefinition(zero)
    )

    // P(r, 0) = 1: only one way along the left column
    val definition = IfThenElseDefinition(
      ArrayElementExpression(ArrayElementExpression(grid, r), c) <= k,
      ExpressionStatement(one),
      subproblemTraversal
    )

    val GWO: EnhancedModel = new EnhancedModel(
      "NumberPathsWithKCoins",
      bound,
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      sol,
      definition,
      answer = ReturnExpressionDefinition(
        SubproblemExpression(Seq(m - one, n - one, k))
      )
    )

    GWO
  }
}