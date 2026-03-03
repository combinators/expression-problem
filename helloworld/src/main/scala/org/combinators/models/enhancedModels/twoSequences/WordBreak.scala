package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

/**
 * Name: Word Break
 * Description:
 * Given a string s and y a dictionary of n words dictionary, check if
 * s can be segmented into a sequence of valid words from the dictionary, separated by spaces.
 *
 * BottomUp returns dp[n] while top-down starts with helper(0) suggesting that these will be
 * two different Bu and TD.
 */

class WordBreak {

  def model: EnhancedModel = {
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val falseLit = new LiteralBoolean(false)

    val s = new ArgExpression(0, "s", StringType(), "i")
    val dict = new ArgExpression(1, "dict", StringArrayType(), "w")

    val bound = List(s, dict)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= new StringLengthExpression(s), new StringLengthExpression(s) + one)

    val helperTable = Map("i" -> i)
    val sol_dt = SubproblemInvocation(Seq("i"), helpers = helperTable)

    // Future Work: Consider adding an IteratorDefinition that shortcircuits definition of a subproblem after iterating for
    // various reasons. Works easily in top down (just return) but not so much in bottom up (might not have ability to break)

//    val iter = IteratorDefinition("w", zero, SelfExpression("w") < new ArrayLengthExpression(dict),
//      new EqualExpression(new SubStringExpression(s, i, i + new StringLengthExpression(new ArrayElementExpression(dict, SelfExpression("w")))),
//        new ArrayElementExpression(dict, SelfExpression("w")), StringType())
//      , SelfExpression("w") + one)
//    val start = i - new StringLengthExpression(new ArrayElementExpression(dict, SelfExpression("w")))

    // THIS IS NOT CORRECT. TRYING SOMETHING TO ENSURE substring works.
    val dt_definition = IfThenElseDefinition(i == zero, ExpressionStatement(falseLit), ExpressionDefinition(s == new SubStringExpression(s, i - one, i))

    )

    val WordBreak = new EnhancedModel("WordBreak",
      bound,
      subproblemType = BooleanType(),    // helper method is an int
      solutionType   = StringType(),     // solution is a string
      sol_dt,
      dt_definition,
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new StringLengthExpression(s))))
    )

    WordBreak
  }
}
