package org.combinators.model.enhancedModels

import org.combinators.model._

/**
 * Name: Unique Paths
 * Description:
 * Given an m x n grid, find the number of unique paths from the top-left
 * to the bottom-right corner. You can only move down or right at each step.
 *
 * Recurrence:
 * P(r, 0) = 1 only one way to reach any cell in the first column (go straight down)
 * P(0, c) = 1 only one way to reach any cell in the first row (go straight right)
 * P(r, c) = P(r-1, c) + P(r, c-1)
 */
class UniquePaths {
  def model: EnhancedModel = {
    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)

    val m = new ArgExpression(0, "m", IntegerType(), "r")
    val n = new ArgExpression(1, "n", IntegerType(), "c")
    val bound = List(m, n)

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") < m, m)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") < n, n)

    val helpers = Map("r" -> r, "c" -> c)
    val sol = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    // P(r, c) = P(r-1, c) + P(r, c-1)
    val subproblemTraversal = ExpressionDefinition(
      new SubproblemExpression(Seq(r - one, c)) + new SubproblemExpression(Seq(r, c - one))
    )

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

    val UP: EnhancedModel = new EnhancedModel(
      "UniquePaths",
      bound,
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      sol,
      definition,
      answer = ReturnExpressionDefinition(
        new SubproblemExpression(Seq(m - one, n - one))
      )
    )

    UP
  }
}