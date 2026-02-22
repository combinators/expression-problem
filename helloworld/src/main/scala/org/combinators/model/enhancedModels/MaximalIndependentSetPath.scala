package org.combinators.model.enhancedModels

import org.combinators.model._

class MaximalIndependentSetPath {
  def model:EnhancedModel = {
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    val path = new ArgExpression(0, "path", IntegerArrayType(), "i")

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= new ArrayLengthExpression(path), new ArrayLengthExpression(path) + one)

    val helpers = Map("i" -> i)
    val soln = SubproblemInvocation(order=Seq("i"), helpers = helpers, returnType = IntegerType())

//    val subproblemTraversal = IfThenElseDefinition(
//      new CharAtExpression(s1, r - one) == new CharAtExpression(s2, c - one),
//      ExpressionStatement(new SubproblemExpression(Seq(r - one, c - one)) + one),
//      ExpressionDefinition(
//        new MaxExpression(
//          new SubproblemExpression(Seq(r, c - one)),
//          new SubproblemExpression(Seq(r - one, c))
//        )
//      )
//    )
    val condition = new LessThanOrEqualExpression(i, zero)

    val subproblem1 = new SubproblemExpression(Seq(i-two))
    val subproblem2 = new SubproblemExpression(Seq(i-one))

    val definition = IfThenElseDefinition(
      condition,
      ExpressionStatement(zero),
      ExpressionDefinition(
        new MaxExpression(
          subproblem1+ new ArrayElementExpression(path, i-one),
          subproblem2
        )
      )

    )



    val MIPS: EnhancedModel = new EnhancedModel(
      "MaimalIndependentSetPath",
      List(path),
      subproblemType = IntegerType(),
      solutionType = StringType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new ArrayLengthExpression(path))))
    )

    MIPS
  }
}
