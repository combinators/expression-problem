package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.any

object arithmetic {
  trait AddOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait SubOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait MultOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait DivOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait ModOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait LtOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait LeOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]

  trait Factory[FT <: operatorExpression.FinalTypes] extends operatorExpression.Factory[FT] {
    def addOp(): AddOp[FT]
    def subOp(): SubOp[FT]
    def multOp(): MultOp[FT]
    def divOp(): DivOp[FT]
    def modOp(): ModOp[FT]
    def ltOp(): LtOp[FT]
    def leOp(): LeOp[FT]

    def add(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(addOp(), left, right)
    def sub(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(subOp(), left, right)
    def mult(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(multOp(), left, right)
    def div(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(divOp(), left, right)
    def mod(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(modOp(), left, right)
    def lt(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(ltOp(), left, right)
    def le(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(leOp(), left, right)
  }
}
