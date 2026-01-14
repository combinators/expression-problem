package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.any

object StringOps {
  trait ToStringOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait AppendStringOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait StringLengthOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def toStringOp(): ToStringOp[FT]
    def appendStringOp(): AppendStringOp[FT]
    def stringLengthOp(): StringLengthOp[FT]

    def toString(exp: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(toStringOp(), exp)
    def appendString(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(appendStringOp(), left, right)
    def stringLength(exp: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(stringLengthOp(), exp)
  }
}