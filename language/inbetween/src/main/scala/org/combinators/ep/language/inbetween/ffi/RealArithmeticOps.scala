package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.any

object RealArithmeticOps {
  trait SqrtOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait PowOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait LogOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait SinOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait CosOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait AbsOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait FloorOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait EulersNumber[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]
  trait Pi[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]

  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def sqrtOp(): SqrtOp[FT]
    def powOp(): PowOp[FT]
    def logOp(): LogOp[FT]
    def sinOp(): SinOp[FT]
    def cosOp(): CosOp[FT]
    def absOp(): AbsOp[FT]
    def floorOp(): FloorOp[FT]
    def pi(): Pi[FT]
    def eulersNumber(): EulersNumber[FT]

    def sqrt(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(sqrtOp(), of)
    def pow(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(powOp(), left, right)
    def log(of: any.Expression[FT], base: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(logOp(), of, base) // FIXME: unaryExpression(logOp(), of)
    def sin(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(sinOp(), of)
    def cos(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(cosOp(), of)
    def abs(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(absOp(), of)
    def floor(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(floorOp(), of)
  }
}