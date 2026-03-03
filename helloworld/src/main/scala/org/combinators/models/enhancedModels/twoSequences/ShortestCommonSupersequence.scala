package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

/**
 * Name: Shortest Common Supersequence
 * Description:
 * Given two strings s1 and s2, find the length of the shortest string
 * that has both s1 and s2 as subsequences.
 *
 * Example:
 * s1 = "abac", s2 = "cab"
 * result = 5 ("cabac")
 *
 * Recurrence:
 * P(0, 0) = 0
 * P(i, 0) = i                              take all of s1 prefix
 * P(0, j) = j                              take all of s2 prefix
 * P(i, j) = P(i-1, j-1) + 1               if s1[i-1] == s2[j-1]
 * P(i, j) = min(P(i-1,j), P(i,j-1)) + 1  otherwise
 */
class ShortestCommonSupersequence {
  def model: EnhancedModel = {
    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)

    val s1 = new ArgExpression(0, "s1", StringType(), "r")
    val s2 = new ArgExpression(1, "s2", StringType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= new StringLengthExpression(s1), new StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= new StringLengthExpression(s2), new StringLengthExpression(s2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    // characters match: include once, inherit diagonal
    // no match: take the shorter option and add 1
    val subproblemTraversal = IfThenElseDefinition(
      new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
      ExpressionStatement(new SubproblemExpression(Seq(r - one, c - one)) + one),
      ExpressionDefinition(
        new MinExpression(
          new SubproblemExpression(Seq(r - one, c)),
          new SubproblemExpression(Seq(r, c - one))
        ) + one
      )
    )

    // P(0, j) = j: take all of s2 prefix
    val baseCase2 = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(c),
      subproblemTraversal
    )

    // P(i, 0) = i: take all of s1 prefix
    val baseCase1 = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(r),
      baseCase2
    )

    // P(0, 0) = 0
    val definition = IfThenElseDefinition(
      r == zero && c == zero,
      ExpressionStatement(zero),
      baseCase1
    )

    val SCS: EnhancedModel = new EnhancedModel(
      "ShortestCommonSupersequence",
      List(s1, s2),
      subproblemType = IntegerType(),
      solutionType = StringType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(
        new SubproblemExpression(Seq(new StringLengthExpression(s1), new StringLengthExpression(s2)))
      )
    )

    SCS
  }
}