package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

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
class MinimumAsciiDelete {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)

    val s1 = ArgExpression(0, "s1", StringType(), "r")
    val s2 = ArgExpression(1, "s2", StringType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= StringLengthExpression(s1), StringLengthExpression(s1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= StringLengthExpression(s2), StringLengthExpression(s2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val asciiS1 = CharToAsciiExpression(CharAtExpression(s1, r - one))
    val asciiS2 = CharToAsciiExpression(CharAtExpression(s2, c - one))

    val subproblemTraversal = IfThenElseDefinition(
      CharAtExpression(s1, r - one) == CharAtExpression(s2, c - one),
      ExpressionStatement(SubproblemExpression(Seq(r - one, c - one))),
      ExpressionDefinition(
        MinExpression(
          SubproblemExpression(Seq(r - one, c)) + asciiS1,
          SubproblemExpression(Seq(r, c - one)) + asciiS2
        )
      )
    )

    val baseCase2 = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(SubproblemExpression(Seq(zero, c - one)) + asciiS2),
      subproblemTraversal
    )

    val baseCase1 = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(SubproblemExpression(Seq(r - one, zero)) + asciiS1),
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
        SubproblemExpression(Seq(StringLengthExpression(s1), StringLengthExpression(s2)))
      )
    )

    MDS
  }
}