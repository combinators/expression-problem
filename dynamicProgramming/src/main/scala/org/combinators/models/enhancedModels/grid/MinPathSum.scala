package org.combinators.models.enhancedModels.grid

import org.combinators.models._

/**
 * Name: Minimum Path Sum
 * Description:
 * Given an m x n grid filled with non-negative numbers, find a path from
 * top-left to bottom-right which minimizes the sum of all numbers along its path.
 * You can only move down or right at each step.
 *
 * Recurrence:
 * P(0, 0) = grid[0][0]
 * P(r, 0) = P(r-1, 0) + grid[r][0]              down left column
 * P(0, c) = P(0, c-1) + grid[0][c]              across top row
 * P(r, c) = min(P(r-1,c), P(r,c-1)) + grid[r][c]
 */
class MinPathSum {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)

    val grid = ArgExpression(0, "grid", IntegerArray2DType(), "g")
    val bound = List(grid)

    val numRows = ArrayLengthExpression(grid)
    val numCols = ArrayLengthExpression(ArrayElementExpression(grid, zero))

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") < numRows, numRows)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") < numCols, numCols)

    val helpers = Map("r" -> r, "c" -> c)
    val sol = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    // grid[r][c]
    val gridVal = ArrayElementExpression(ArrayElementExpression(grid, r), c)

    // P(r, c) = min(P(r-1, c), P(r, c-1)) + grid[r][c]
    val general = ExpressionDefinition(
      MinExpression(
        SubproblemExpression(Seq(r - one, c)),
        SubproblemExpression(Seq(r, c - one))
      ) + gridVal
    )

    // P(0, c) = P(0, c-1) + grid[0][c]
    val topRow = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(SubproblemExpression(Seq(zero, c - one)) + gridVal),
      general
    )

    // P(r, 0) = P(r-1, 0) + grid[r][0]
    val leftCol = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(SubproblemExpression(Seq(r - one, zero)) + gridVal),
      topRow
    )

    // P(0, 0) = grid[0][0]
    val definition = IfThenElseDefinition(
      r == zero && c == zero,
      ExpressionStatement(gridVal),
      leftCol
    )

    val MPS: EnhancedModel = new EnhancedModel(
      "MinPathSum",
      bound,
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      sol,
      definition,
      answer = ReturnExpressionDefinition(
        SubproblemExpression(Seq(numRows - one, numCols - one))
      )
    )

    MPS
  }
}