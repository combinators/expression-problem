package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.any

object realArithmetic {
  trait SqrtOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait PowOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait LogOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait SinOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait CosOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait AbsOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait FloorOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]

  trait EulersNumber[FT <: operatorExpression.FinalTypes] extends any.Expression[FT]

  trait Pi[FT <: operatorExpression.FinalTypes] extends any.Expression[FT]


  trait Factory[FT <: operatorExpression.FinalTypes] extends operatorExpression.Factory[FT] {
    def sqrtOp(): SqrtOp[FT]
    def powOp(): PowOp[FT]
    def logOp(): LogOp[FT]
    def sinOp(): SinOp[FT]
    def cosOp(): CosOp[FT]
    def absOp(): AbsOp[FT]
    def floorOp(): FloorOp[FT]

    def pi(): Pi[FT]
    def eulersNumber(): EulersNumber[FT]

    def sqrt(of: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(sqrtOp(), of)
    def pow(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(powOp(), left, right)
    def log(of: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(logOp(), of)
    def sin(of: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(sinOp(), of)
    def cos(of: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(cosOp(), of)
    def abs(of: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(absOp(), of)
    def floor(of: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(floorOp(), of)
  }
}
