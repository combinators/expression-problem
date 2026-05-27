package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

/**
 * Name: Distinct Subsequences
 * Description:
 * Given two strings s1 and s2, return the number of distinct subsequences of s1 which equals s2.
 *
 * Example:
 * s = "rabbbit", t = "rabbit"
 * result = 3 (three ways to pick the three b's)
 *
 * Recurrence:
 *   P(i, 0) = 1           (empty t is always a subsequence)
 *   P(0, j) = 0, j > 0   (non-empty t cannot be a subsequence of empty s)
 *   P(i, j) = P(i-1, j)                    if s[i-1] != t[j-1]
 *   P(i, j) = P(i-1, j) + P(i-1, j-1)     if s[i-1] == t[j-1]
 */

// todo: identify root cause of test case error
class DistinctSubsequences {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one  = LiteralInt(1)

    val s1 = ArgExpression(0, "s1", StringType(), "r")
    val s2 = ArgExpression(1, "s2", StringType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= StringLengthExpression(s1), StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= StringLengthExpression(s2), StringLengthExpression(s2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val matchCase = SubproblemExpression(Seq(r - one, c - one)) + SubproblemExpression(Seq(r - one, c))

    val noMatchCase = ExpressionDefinition(
      SubproblemExpression(Seq(r - one, c))
    )

    val subproblemTraversal = IfThenElseDefinition(
      CharAtExpression(s1, r - one) == CharAtExpression(s2, c - one),
      ExpressionStatement(matchCase),
      noMatchCase
    )

    val baseCase2 = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(one),
      subproblemTraversal
    )

    val definition = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(zero),
      baseCase2
    )

    val DS: EnhancedModel = new EnhancedModel(
      "DistinctSubsequences",
      List(s1, s2),
      subproblemType = IntegerType(),
      solutionType   = IntegerType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(
        SubproblemExpression(Seq(StringLengthExpression(s1), StringLengthExpression(s2)))
      )
    )

    DS
  }
}