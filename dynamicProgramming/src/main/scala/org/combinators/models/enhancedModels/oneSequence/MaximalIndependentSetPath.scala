package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class MaximalIndependentSetPath {
  def model:EnhancedModel = {
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val two: LiteralInt = LiteralInt(2)

    val path = ArgExpression(0, "path", IntegerArrayType(), "i")

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= ArrayLengthExpression(path), ArrayLengthExpression(path) + one)

    val helpers = Map("i" -> i)
    val soln = SubproblemInvocation(order=Seq("i"), helpers = helpers, returnType = IntegerType())

//    val subproblemTraversal = IfThenElseDefinition(
//      CharAtExpression(s1, r - one) == CharAtExpression(s2, c - one),
//      ExpressionStatement(SubproblemExpression(Seq(r - one, c - one)) + one),
//      ExpressionDefinition(
//        MaxExpression(
//          SubproblemExpression(Seq(r, c - one)),
//          SubproblemExpression(Seq(r - one, c))
//        )
//      )
//    )
    val condition1 = LessThanOrEqualExpression(i, zero)
    val condition2 = LessThanOrEqualExpression(i, one)

    val subproblem1 = SubproblemExpression(Seq(i - two))
    val subproblem2 = SubproblemExpression(Seq(i - one))

    val subDefinition= IfThenElseDefinition(
      condition2,
      ExpressionStatement(ArrayElementExpression(path, i - one)),
      ExpressionDefinition(
        MaxExpression(
          subproblem1 + ArrayElementExpression(path, i - one),
          subproblem2
        )
      )
    )

    val definition = IfThenElseDefinition(
      condition1,
      ExpressionStatement(zero),
      subDefinition
    )

    val MIPS: EnhancedModel = new EnhancedModel(
      "MaximalIndependentSetPath",
      List(path),
      subproblemType = IntegerType(),
      solutionType = StringType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(ArrayLengthExpression(path))))
    )

    MIPS
  }
}
