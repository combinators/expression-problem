package org.combinators.ep.language.inbetween.ffi

trait AssertionsAST extends OperatorExpressionOpsAST  {
  object assertionOps {
    trait AssertTrueOp extends operatorExpressions.Operator

    trait Factory {
      def assertTrueOp(): AssertTrueOp

      def assertTrue(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(assertTrueOp(), exp)
    }
  }
  val assertionOpsFactory: assertionOps.Factory
}
