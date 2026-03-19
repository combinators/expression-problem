package org.combinators.ep.language.inbetween.ffi

trait StringAST extends OperatorExpressionOpsAST  {
  object stringOps {
    trait AppendStringOp extends operatorExpressions.Operator
    trait GetCharAtOp extends operatorExpressions.Operator
    trait StringLengthOp extends operatorExpressions.Operator
    trait SubStringOp extends operatorExpressions.Operator
    trait ToStringOp extends operatorExpressions.Operator


    trait Factory {
      def appendStringOp(): AppendStringOp
      def getCharAtOp(): GetCharAtOp
      def stringLengthOp(): StringLengthOp
      def subStringOp(): SubStringOp
      def toStringOp(): ToStringOp

      def appendString(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(appendStringOp(), left, right)

      def getCharAt(base: any.Expression, idx: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(subStringOp(), base, idx)
      def stringLength(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(stringLengthOp(), exp)
      def subString(base:any.Expression, left: any.Expression, right:any.Expression): operatorExpressions.TernaryExpression =
        operatorExpressionsFactory.ternaryExpression(subStringOp(), base, left, right)
      def toString(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(toStringOp(), exp)
    }
  }
  val stringOpsFactory: stringOps.Factory
}