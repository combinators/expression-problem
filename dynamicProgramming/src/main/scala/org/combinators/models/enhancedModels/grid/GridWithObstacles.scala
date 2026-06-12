package org.combinators.models.enhancedModels.grid

import org.combinators.models.*

/**
 * Name: Grid With Obstacles
 * Description:
 * Given an m x n grid, find the number of unique paths from the top-left
 * to the bottom-right corner. You can only move down or right at each step, and you
 * must avoid cells containing a 1 (obstacle). This is a variation of UniquePaths
 *
 * Recurrence:
 * P(r, 0) = 1 only one way to reach any cell in the first column (go straight down)
 * P(0, c) = 1 only one way to reach any cell in the first row (go straight right)
 * P(r, c) = P(r-1, c) + P(r, c-1)
 */
class GridWithObstacles {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)

    val grid = ArgExpression(0, "grid", IntegerArray2DType(), "g")

    val m = ArrayLengthExpression(grid)
    val n = ArrayLengthExpression(ArrayElementExpression(grid, zero))
    val bound = List(grid)

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") < m, m)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") < n, n)

    val helpers = Map("r" -> r, "c" -> c)
    val sol = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    // if grid[r][c] = 0 then P(r, c) = P(r-1, c) + P(r, c-1), otherwise must equal 0
    val subproblemTraversal = IfThenElseDefinition(ArrayElementExpression(ArrayElementExpression(grid, r), c) == one,
      ExpressionStatement(zero),
      ExpressionDefinition(
      SubproblemExpression(Seq(r - one, c)) + SubproblemExpression(Seq(r, c - one))
    ))

    // P(0, c) = 1: only one way along the top row
    val baseCase2 = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(one),
      subproblemTraversal
    )

    // P(r, 0) = 1: only one way along the left column
    val definition = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(one),
      baseCase2
    )

    val GWO: EnhancedModel = new EnhancedModel(
      "GridWithObstacles",
      bound,
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      sol,
      definition,
      answer = ReturnExpressionDefinition(
        SubproblemExpression(Seq(m - one, n - one))
      )
    )

    GWO
  }
}