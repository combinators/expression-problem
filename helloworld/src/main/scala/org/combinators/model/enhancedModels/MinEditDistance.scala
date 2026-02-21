package org.combinators.model.enhancedModels

import org.combinators.model._

class MinEditDistance {
  def model: EnhancedModel = {
      {
        val zero = new LiteralInt(0)
        val one = new LiteralInt(1)

        val s1 = new ArgExpression(0, "s1", StringType(), "r")
        val s2 = new ArgExpression(1, "s2", StringType(), "c")

        val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= new StringLengthExpression(s1), new StringLengthExpression(s1) + one)
        val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= new StringLengthExpression(s2), new StringLengthExpression(s2) + one)

        val helpers = Map("r" -> r, "c" -> c)
        val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

        val subproblemTraversal = IfThenElseDefinition(
          new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
          ExpressionStatement(new SubproblemExpression(Seq(r - one, c - one))),
          ExpressionDefinition(
            new MinExpression(
              new SubproblemExpression(Seq(r - one, c)),
              new MinExpression(
                new SubproblemExpression(Seq(r, c - one)),
                new SubproblemExpression(Seq(r - one, c - one))
              )
            ) + one
          )
        )

        val baseCase2 = IfThenElseDefinition(
          r == zero,
          ExpressionStatement(r),
          subproblemTraversal
        )

        val definition = IfThenElseDefinition(
          c == zero,
          ExpressionStatement(c),
          baseCase2
        )

        val MED: EnhancedModel = new EnhancedModel(
          "MinEditDistance",
          List(s1, s2),
          subproblemType = IntegerType(),
          solutionType = StringType(),
          soln,
          definition,
          answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new StringLengthExpression(s1), new StringLengthExpression(s2))))
        )

        MED
      }
  }
}
