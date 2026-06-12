package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

/**
 * Name: Wildcard Pattern Matching
 * Description:
 * Given strings txt and pat, return whether pat matches txt.
 * '?' matches any single character.
 * '*' matches any sequence of characters (including empty).
 *
 * Recurrence:
 * P(0, 0) = true
 * P(0, j) = (pat[j-1] == '*') && P(0, j-1)    only a run of '*'s can match empty txt
 * P(i, 0) = false                               non-empty pattern can't match empty txt (unless all '*')
 * P(i, j) = P(i-1, j-1)                        if pat[j-1] == '?' or txt[i-1] == pat[j-1]
 * P(i, j) = P(i-1, j) || P(i, j-1)             if pat[j-1] == '*'
 * P(i, j) = false                               otherwise
 */

// todo: identify root cause of test case error
class WildcardPatternMatching {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)
    val star = LiteralChar('*')
    val quest = LiteralChar('?')

    val txt = ArgExpression(0, "txt", StringType(), "r")
    val pat = ArgExpression(1, "pat", StringType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= StringLengthExpression(txt), StringLengthExpression(txt) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= StringLengthExpression(pat), StringLengthExpression(pat) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = BooleanType())

    val patChar = CharAtExpression(pat, c - one)
    val txtChar = CharAtExpression(txt, r - one)

    val isStar = patChar == star
    val isQuest = patChar == quest
    val charsMatch = txtChar == patChar

    // P(i,j) when pat[j-1] == '*': match empty (advance pat) or consume txt char
    val starCase = OrExpression(
      SubproblemExpression(Seq(r - one, c)), // '*' consumes one txt char
      SubproblemExpression(Seq(r, c - one)) // '*' matches empty
    )

    // P(i,j) when pat[j-1] == '?' or chars match: inherit diagonal
    val matchCase = ExpressionDefinition(SubproblemExpression(Seq(r - one, c - one)))

    // P(i,j) general: star, match/'?', or false
    val subproblemTraversal = IfThenElseDefinition(
      isStar,
      ExpressionStatement(starCase),
      IfThenElseDefinition(
        isQuest || charsMatch,
        ExpressionStatement(SubproblemExpression(Seq(r - one, c - one))),
        ExpressionDefinition(LiteralBoolean(false))
      )
    )

    // P(0, j) = (pat[j-1] == '*') && P(0, j-1)
    val baseRow = IfThenElseDefinition(
      r == zero,
      ExpressionStatement(isStar && SubproblemExpression(Seq(zero, c - one))),
      subproblemTraversal
    )

    // P(i, 0) = false  (non-empty txt, empty pat)
    val baseCol = IfThenElseDefinition(
      c == zero,
      ExpressionStatement(LiteralBoolean(false)),
      baseRow
    )

    // P(0, 0) = true
    val definition = IfThenElseDefinition(
      r == zero && c == zero,
      ExpressionStatement(LiteralBoolean(true)),
      baseCol
    )

    val WPM: EnhancedModel = new EnhancedModel(
      "WildcardPatternMatching",
      List(txt, pat),
      subproblemType = BooleanType(),
      solutionType = BooleanType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(
        SubproblemExpression(Seq(StringLengthExpression(txt), StringLengthExpression(pat)))
      )
    )

    WPM
  }
}