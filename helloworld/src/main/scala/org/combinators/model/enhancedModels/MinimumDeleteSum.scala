package org.combinators.model.enhancedModels

import org.combinators.model._

// todo: add ascii implementation in engine

/**
 * Name: Minimum Delete Sum
 * Description:
 * Given two strings s1 and s2, return the lowest ASCII sum of deleted characters
 * to make them equal.
 *
 * Example:
 * s1 = "sea", s2 = "eat"
 * result = 231 (delete 's' (115) from s1 and 't' (116) from s2)
 *
 * Recurrence:
 * P(0, 0) = 0
 * P(i, 0) = P(i-1, 0) + ascii(s1[i-1])     delete all of s1 prefix
 * P(0, j) = P(0, j-1) + ascii(s2[j-1])     delete all of s2 prefix
 * P(i, j) = P(i-1, j-1)                                          if s1[i-1] == s2[j-1]
 * P(i, j) = min(P(i-1,j) + ascii(s1[i-1]), P(i,j-1) + ascii(s2[j-1]))  otherwise
 *
 */
class MinimumDeleteSum {
  def model: EnhancedModel = {
    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)

    val s1 = new ArgExpression(0, "s1", StringType(), "r")
    val s2 = new ArgExpression(1, "s2", StringType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= new StringLengthExpression(s1), new StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= new StringLengthExpression(s2), new StringLengthExpression(s2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val asciiS1 = new CharToAsciiExpression(new CharAtExpression(s1, r - one))
    val asciiS2 = new CharToAsciiExpression(new CharAtExpression(s2, c - one))

    val subproblemTraversal = IfThenElseDefinition(
      new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
      ExpressionStatement(new SubproblemExpression(Seq(r - one, c - one))),
      ExpressionDefinition(
        new MinExpression(
          new SubproblemExpression(Seq(r - one, c)) + asciiS1,
          new SubproblemExpression(Seq(r, c - one)) + asciiS2
        )
      )
    )

    val baseCase2 = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(new SubproblemExpression(Seq(zero, c - one)) + asciiS2),
      subproblemTraversal
    )

    val baseCase1 = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(new SubproblemExpression(Seq(r - one, zero)) + asciiS1),
      baseCase2
    )

    val definition = IfThenElseDefinition(
      r == zero && c == zero,
      ExpressionStatement(zero),
      baseCase1
    )

    val MDS: EnhancedModel = new EnhancedModel(
      "MinimumDeleteSum",
      List(s1, s2),
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(
        new SubproblemExpression(Seq(new StringLengthExpression(s1), new StringLengthExpression(s2)))
      )
    )

    MDS
  }
}