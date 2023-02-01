package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.any

object boolean {
  trait AndOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait OrOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]

  trait NotOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]

  trait True[FT <: operatorExpression.FinalTypes] extends any.Expression[FT]
  trait False[FT <: operatorExpression.FinalTypes] extends any.Expression[FT]

  trait Factory[FT <: operatorExpression.FinalTypes] extends operatorExpression.Factory[FT] {
    def andOp(): AndOp[FT]
    def orOp(): OrOp[FT]
    def notOp(): NotOp[FT]

    def trueExp(): True[FT]
    def falseExp(): False[FT]
    def and(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(andOp(), left, right)
    def or(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(orOp(), left, right)
    def not(exp: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(notOp(), exp)
  }
}