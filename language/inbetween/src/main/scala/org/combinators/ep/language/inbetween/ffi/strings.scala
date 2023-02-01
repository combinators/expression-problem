package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.any

object strings {
  trait ToStringOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait AppendStringOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]
  trait StringLengthOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]


  trait Factory[FT <: operatorExpression.FinalTypes] extends operatorExpression.Factory[FT] {
    def toStringOp(): ToStringOp[FT]
    def appendStringOp(): AppendStringOp[FT]
    def stringLengthOp(): StringLengthOp[FT]

    def toString(exp: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(toStringOp(), exp)
    def appendString(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(appendStringOp(), left, right)
    def stringLength(exp: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(stringLengthOp(), exp)
  }
}