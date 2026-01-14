package org.combinators.ep.language.inbetween.ffi

trait StringAST extends OperatorExpressionOpsAST  {
  object stringOps {
    trait ToStringOp extends operatorExpressions.Operator
    trait AppendStringOp extends operatorExpressions.Operator
    trait StringLengthOp extends operatorExpressions.Operator


    trait Factory {
      def toStringOp(): ToStringOp
      def appendStringOp(): AppendStringOp
      def stringLengthOp(): StringLengthOp

      def toString(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(toStringOp(), exp)
      def appendString(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(appendStringOp(), left, right)
      def stringLength(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(stringLengthOp(), exp)
    }
  }
  val stringOpsFactory: stringOps.Factory
}