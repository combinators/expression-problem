package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

/**
 * Name: Word Break
 * Description:
 * Given a string s and y a dictionary of n words dictionary, check if
 * s can be segmented into a sequence of valid words from the dictionary, separated by spaces.
 *
 * BROKEN. Will only work when you can support validating that a raw array contains a specific value
 */
@deprecated(message = "BROKEN. Will only work when possible to add code to confirm a string is a member of an array")
class WordBreak {

  def model: EnhancedModel = {
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val falseLit = LiteralBoolean(false)

    val s = ArgExpression(0, "s", StringType(), "i")
    val dict = ArgExpression(1, "dict", StringArrayType(), "w")

    val bound = List(s, dict)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= StringLengthExpression(s), StringLengthExpression(s) + one)

    val helperTable = Map("i" -> i)
    val sol_dt = SubproblemInvocation(Seq("i"), helpers = helperTable)

    // Future Work: Consider adding an IteratorDefinition that shortcircuits definition of a subproblem after iterating for
    // various reasons. Works easily in top down (just return) but not so much in bottom up (might not have ability to break)

//    val iter = IteratorDefinition("w", zero, SelfExpression("w") < ArrayLengthExpression(dict),
//      EqualExpression(SubStringExpression(s, i, i + StringLengthExpression(ArrayElementExpression(dict, SelfExpression("w")))),
//        ArrayElementExpression(dict, SelfExpression("w")), StringType())
//      , SelfExpression("w") + one)
//    val start = i - StringLengthExpression(ArrayElementExpression(dict, SelfExpression("w")))

    // TO DO: THIS IS NOT CORRECT. You need to confirm substring exists within dict
    val dt_definition = IfThenElseDefinition(i == zero, ExpressionStatement(falseLit),
      ExpressionDefinition(EqualExpression(s, SubStringExpression(s, i - one, i), StringType()))

    )

    val WordBreak = new EnhancedModel("WordBreak",
      bound,
      subproblemType = BooleanType(),    // helper method is an int
      solutionType   = StringType(),     // solution is a string
      sol_dt,
      dt_definition,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(StringLengthExpression(s))))
    )

    WordBreak
  }
}
