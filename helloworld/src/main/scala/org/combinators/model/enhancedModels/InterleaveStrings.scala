package org.combinators.model.enhancedModels

import org.combinators.model._

class InterleaveStrings {
  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    // Interleaving strings really explores space over (s1,s2) and uses s3 as a target so it isn't part of the search space.
    val s1 = new ArgExpression(0, "s1", StringType(), "i1")
    val s2 = new ArgExpression(1, "s2", StringType(), "i2")
    val s3 = new ArgExpression(2, "s3", StringType(), "i3")
    val bounds = List(s1, s2, s3)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i1: HelperExpression = HelperExpression("i1", zero, SelfExpression("i1") <= new StringLengthExpression(s1), new StringLengthExpression(s1) + one)
    val i2: HelperExpression = HelperExpression("i2", zero, SelfExpression("i2") <= new StringLengthExpression(s2), new StringLengthExpression(s2) + one)

    // what the compute() method calls with helper(s1.length(), s2.length())
    val helpers = Map("i1" -> i1, "i2" -> i2)
    val sol = SubproblemInvocation(order=Seq("i1", "i2"), helpers = helpers, returnType = BooleanType())

    /*
     *   P(i,j) = 0, if i == j
     *   P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
     *      for (int k = i; k < j; k++)
     */

    val case_final = new OrExpression(
      new AndExpression(
        new CharAtExpression(s1, i1 - one) == new CharAtExpression(s3, i1 + i2 - one),
        new SubproblemExpression(Seq(i1 - one, i2))
      ),
      new AndExpression(
        new CharAtExpression(s2, i2 - one) == new CharAtExpression(s3, i1 + i2 - one),
        new SubproblemExpression(Seq(i1, i2 - one))
      )
    )

    val case_5 = IfThenElseDefinition(i2 == zero,
      ExpressionStatement((new CharAtExpression(s1, i1 - one) == new CharAtExpression(s3, i1 - one)) && new SubproblemExpression(Seq(i1 - one, zero))),
      ExpressionDefinition(case_final))

    val case_4 = IfThenElseDefinition(i1 == zero,
      ExpressionStatement((new CharAtExpression(s2, i2 - one) == new CharAtExpression(s3, i2 - one)) && new SubproblemExpression(Seq(zero, i2 - one))),
      case_5)

    val case_3 = IfThenElseDefinition((i1 == zero) && (i2==zero),
      ExpressionStatement(new LiteralBoolean(true)),
      case_4)

    val case_2 = IfThenElseDefinition(new StringLengthExpression(s3) < i1 + i2,
      ExpressionStatement(new LiteralBoolean(false)),
      case_3)

    val case_1 = IfThenElseDefinition(new StringLengthExpression(s2) < i2,
      ExpressionStatement(new LiteralBoolean(false)),
      case_2)

    val ils_definition = IfThenElseDefinition(new StringLengthExpression(s1) < i1,
      ExpressionStatement(new LiteralBoolean(false)),
      case_1)

    val ILS = new EnhancedModel("InterleavingStrings",
      bounds,
      subproblemType = BooleanType(),    // helper() method returns boolean
      solutionType = StringType(),       // solution is a string, showing where characters come from S1 with parens
      sol,
      ils_definition,
      answer = new SubproblemExpression(Seq(new StringLengthExpression(s1), new StringLengthExpression(s2)))
    )

    ILS
  }
}
