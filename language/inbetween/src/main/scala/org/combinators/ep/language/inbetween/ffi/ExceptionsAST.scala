package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

// need to change to   trait RaiseOp extends any.Statement instead.
// add final types, like with arrays/maps

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
