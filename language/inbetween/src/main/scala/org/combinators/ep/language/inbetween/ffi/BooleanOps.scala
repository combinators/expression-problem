package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.any

object BooleanOps {
  trait AndOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait OrOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait NotOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait True[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]
  trait False[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]

  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def andOp(): AndOp[FT]
    def orOp(): OrOp[FT]
    def notOp(): NotOp[FT]

    def trueExp(): True[FT]
    def falseExp(): False[FT]
    def and(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(andOp(), left, right)
    def or(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(orOp(), left, right)
    def not(exp: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(notOp(), exp)
  }
}