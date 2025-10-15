package org.combinators.ep.language.inbetween.ffi

trait ArithmeticOpsAST extends OperatorExpressionOpsAST {
  object arithmeticOps {
    trait AddOp extends operatorExpressions.Operator
    trait SubOp extends operatorExpressions.Operator
    trait MultOp extends operatorExpressions.Operator
    trait DivOp extends operatorExpressions.Operator
    trait ModOp extends operatorExpressions.Operator
    trait LtOp extends operatorExpressions.Operator
    trait LeOp extends operatorExpressions.Operator

    trait Factory {
      def addOp(): AddOp
      def subOp(): SubOp
      def multOp(): MultOp
      def divOp(): DivOp
      def modOp(): ModOp
      def ltOp(): LtOp
      def leOp(): LeOp

      def add(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(addOp(), left, right)
      def sub(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(subOp(), left, right)
      def mult(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(multOp(), left, right)
      def div(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(divOp(), left, right)
      def mod(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(modOp(), left, right)
      def lt(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(ltOp(), left, right)
      def le(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(leOp(), left, right)
    }
  }
  
  val arithmeticOpsFactory: arithmeticOps.Factory
}