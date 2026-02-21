package org.combinators.model.enhancedModels

import org.combinators.model._

class UncrossedLines {
  def model: EnhancedModel = {
    val zero = new LiteralInt(0)
    val one = new LiteralInt(1)

    val nums1 = new ArgExpression(0, "nums1", IntegerArrayType(), "r")
    val nums2 = new ArgExpression(1, "nums2", IntegerArrayType(), "c")

    val r: HelperExpression = HelperExpression("r", zero, SelfExpression("r") <= new ArrayLengthExpression(nums1), new ArrayLengthExpression(nums1) + one)
    val c: HelperExpression = HelperExpression("c", zero, SelfExpression("c") <= new ArrayLengthExpression(nums2), new ArrayLengthExpression(nums2) + one)

    val helpers = Map("r" -> r, "c" -> c)
    val soln = SubproblemInvocation(order = Seq("r", "c"), helpers = helpers, returnType = IntegerType())

    val subproblemTraversal = IfThenElseDefinition(
      new ArrayElementExpression(nums1, r - one) == new ArrayElementExpression(nums2, c - one),
      ExpressionStatement(new SubproblemExpression(Seq(r - one, c - one)) + one),
      ExpressionDefinition(
        new MaxExpression(
          new SubproblemExpression(Seq(r, c - one)),
          new SubproblemExpression(Seq(r - one, c))
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
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(new ArrayLengthExpression(nums1), new ArrayLengthExpression(nums2))))
    )

    UL
  }
}
