package org.combinators.ep.language.inbetween.ffi

trait RealArithmeticAST extends OperatorExpressionOpsAST  {
  object realArithmeticOps {
    trait SqrtOp extends operatorExpressions.Operator

    trait PowOp extends operatorExpressions.Operator

    trait LogOp extends operatorExpressions.Operator

    trait SinOp extends operatorExpressions.Operator

    trait CosOp extends operatorExpressions.Operator

    trait AbsOp extends operatorExpressions.Operator

    trait FloorOp extends operatorExpressions.Operator

    trait EulersNumber extends any.Expression

    trait Pi extends any.Expression


    trait Factory {
      def sqrtOp(): SqrtOp
      def powOp(): PowOp
      def logOp(): LogOp
      def sinOp(): SinOp
      def cosOp(): CosOp
      def absOp(): AbsOp
      def floorOp(): FloorOp

      def pi(): Pi
      def eulersNumber(): EulersNumber

      def sqrt(of: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(sqrtOp(), of)
      def pow(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(powOp(), left, right)

      def log(of: any.Expression, base: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(logOp(), of, base) // FIXME: unaryExpression(logOp(), of)
      def sin(of: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(sinOp(), of)
      def cos(of: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(cosOp(), of)
      def abs(of: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(absOp(), of)
      def floor(of: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(floorOp(), of)
    }
  }
  val realArithmeticOpsFactory: realArithmeticOps.Factory
}