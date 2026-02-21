package org.combinators.model.enhancedModels

import org.combinators.model._

class ThreeStringLCS {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val s1  = new ArgExpression(0, "s1", StringType(), "i")
    val s2  = new ArgExpression(1, "s2", StringType(), "j")
    val s3  = new ArgExpression(2, "s3", StringType(), "k")
    val bounds = List(s1, s2, s3)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = HelperExpression("i", one, SelfExpression("i") <= new StringLengthExpression(s1), new StringLengthExpression(s1) + one)
    val j: HelperExpression = HelperExpression("j", one, SelfExpression("j") <= new StringLengthExpression(s2), new StringLengthExpression(s2) + one)
    val k: HelperExpression = HelperExpression("k", one, SelfExpression("k") <= new StringLengthExpression(s3), new StringLengthExpression(s3) + one)

    // what the compute() method calls with helper(s1.length(), s2.length())
    val helpers = Map("i" -> i, "j" -> j, "k" -> k)
    val sol = SubproblemInvocation(order=Seq("i", "j", "k"), helpers = helpers, returnType = IntegerType())

    /*
     *   P(i,j,k) = 0, if i == 0 || j == 0 || k == 0 for all Ranges
     *   P(i,j,k) = Max of three sub-cases
     */
    val recursive_case = new MaxExpression(new SubproblemExpression(Seq(i - one, j, k)),
      new MaxExpression(new SubproblemExpression(Seq(i, j - one, k)),
        new SubproblemExpression(Seq(i, j, k - one))))

    val strings_case = IfThenElseDefinition(new CharAtExpression(s1, i - one) == new CharAtExpression(s2, j - one) &&
      new CharAtExpression(s2, j - one) == new CharAtExpression(s3, k - one),
      ExpressionStatement(new SubproblemExpression(Seq(i - one, j - one, k - one)) + one),
      ExpressionDefinition(recursive_case))

    val tslcs_definition = IfThenElseDefinition(i == zero || j == zero || k == zero, ExpressionStatement(zero), strings_case)

    val TSLCS = new EnhancedModel("ThreeStringLCS",
      bounds,
      subproblemType = IntegerType(),         // helper() method returns int
      solutionType   = StringType(),          // solution is a string, showing where characters come from S1 with parens
      sol,
      tslcs_definition,
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new StringLengthExpression(s1), new StringLengthExpression(s2), new StringLengthExpression(s3))))
    )

    TSLCS
  }
}
