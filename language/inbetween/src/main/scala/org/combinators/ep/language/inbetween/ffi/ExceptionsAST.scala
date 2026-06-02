package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

trait ExceptionsAST extends OperatorExpressionOpsAST  {
  object exceptionsOps {
    trait RaiseOp extends operatorExpressions.Operator

    trait Factory {
      def raiseOp(): RaiseOp

      def raiseOp(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(raiseOp(), exp)
    }
  }
  val exceptionsOpsFactory: exceptionsOps.Factory
}
