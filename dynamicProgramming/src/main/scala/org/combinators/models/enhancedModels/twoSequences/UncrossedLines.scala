package org.combinators.models.enhancedModels.twoSequences

import org.combinators.models._

class UncrossedLines {
  def model: EnhancedModel = {
    val zero = LiteralInt(0)
    val one = LiteralInt(1)

    val nums1 = ArgExpression(0, "nums1", IntegerArrayType(), "r")
    val nums2 = ArgExpression(1, "nums2", IntegerArrayType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= ArrayLengthExpression(nums1), ArrayLengthExpression(nums1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= ArrayLengthExpression(nums2), ArrayLengthExpression(nums2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val subproblemTraversal = IfThenElseDefinition(
      ArrayElementExpression(nums1, r - one) == ArrayElementExpression(nums2, c - one),
      ExpressionStatement(SubproblemExpression(Seq(r - one, c - one)) + one),
      ExpressionDefinition(
        MaxExpression(
          SubproblemExpression(Seq(r, c - one)),
          SubproblemExpression(Seq(r - one, c))
        )
      )
    )

    val definition = IfThenElseDefinition(
      r == zero || c == zero,
      ExpressionStatement(zero),
      subproblemTraversal
    )

    val UL: EnhancedModel = new EnhancedModel(
      "UncrossedLines",
      List(nums1, nums2),
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      soln,
      definition,
      answer = ReturnExpressionDefinition(SubproblemExpression(Seq(ArrayLengthExpression(nums1), ArrayLengthExpression(nums2))))
    )

    UL
  }
}
