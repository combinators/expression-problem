package org.combinators.ep.language.inbetween.ffi

trait BooleanAST extends OperatorExpressionOpsAST  {
  object booleanOps {
    trait AndOp extends operatorExpressions.Operator

    trait OrOp extends operatorExpressions.Operator

    trait NotOp extends operatorExpressions.Operator

    trait True extends any.Expression

    trait False extends any.Expression

    trait Factory {
      def andOp(): AndOp
      def orOp(): OrOp
      def notOp(): NotOp

      def trueExp(): True
      def falseExp(): False
      def and(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(andOp(), left, right)
      def or(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(orOp(), left, right)
      def not(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(notOp(), exp)
    }
  }
  val booleanOpsFactory: booleanOps.Factory
}