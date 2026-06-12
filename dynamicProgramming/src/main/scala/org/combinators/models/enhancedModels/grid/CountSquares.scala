package org.combinators.models.enhancedModels.grid

import org.combinators.models._

/**
 * Name: Count Squares
 * Description:
 * Given an m x n binary matrix, return how many square submatrices have all ones.
 *
 * Recurrence:
 *   P(r, c) = 0                                          if grid[r][c] == 0
 *   P(r, c) = 1                                          if grid[r][c] == 1 and (r==0 || c==0)
 *   P(r, c) = min(P(r-1,c-1), P(r-1,c), P(r,c-1)) + 1  otherwise
 */
class CountSquares {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)

    val grid = ArgExpression(0, "grid", IntegerArray2DType(), "g")

    val numRows = ArrayLengthExpression(grid)
    val numCols = ArrayLengthExpression(ArrayElementExpression(grid, zero))

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") < numRows, numRows)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") < numCols, numCols)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    // grid[r][c]
    val gridVal = ArrayElementExpression(ArrayElementExpression(grid, r), c)

    // min(P(r-1,c-1), min(P(r-1,c), P(r,c-1))) + 1
    val minThree = MinExpression(
      SubproblemExpression(Seq(r - one, c - one)),
      MinExpression(
        SubproblemExpression(Seq(r - one, c)),
        SubproblemExpression(Seq(r, c - one))
      )
    ) + one

    // grid[r][c] == 1: edge cells get 1, interior cells get minThree
    val onesCase = IfThenElseDefinition(
      r == zero || c == zero,
      ExpressionStatement(one),
      ExpressionDefinition(minThree)
    )

    // grid[r][c] == 0: no squares possible here
    val definition = IfThenElseDefinition(
      gridVal == zero,
      ExpressionStatement(zero),
      onesCase
    )

    val final_answer = ReturnAccumulatedDefinition(
      "sum",
      Seq(
        AccumulatorInformation("ridx", zero, SelfExpression("ridx") < numRows, SelfExpression("ridx") + one),
        AccumulatorInformation("cidx", zero, SelfExpression("cidx") < numCols, SelfExpression("cidx") + one),
      ),
      SubproblemExpression(Seq(SelfExpression("ridx"), SelfExpression("cidx")))
    )

    val CS: EnhancedModel = new EnhancedModel(
      "CountSquares",
      List(grid),
      subproblemType = IntegerType(),
      solutionType   = IntegerType(),
      soln,
      definition,
      answer = final_answer   // figure out how to sum elements using expressions
    )

    CS
  }
}