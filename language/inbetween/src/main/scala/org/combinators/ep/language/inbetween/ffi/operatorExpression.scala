package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.any

object operatorExpression {
  trait FinalTypes extends any.FinalTypes {
    type Operator
    type BinaryExpression <: Expression
    type UnaryExpression <: Expression
  }

  trait BinaryExpression[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfBinaryExpression: finalTypes.BinaryExpression
    def operator: Operator[FT]
    def left: any.Expression[FT]
    def right: any.Expression[FT]
  }

  trait UnaryExpression[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfUnaryExpression: finalTypes.UnaryExpression
    def operator: Operator[FT]
    def operand: any.Expression[FT]
  }

  trait Operator[FT <: FinalTypes] extends Factory[FT] {
    def getSelfOperator: finalTypes.Operator
  }

  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {

    def operator(name: any.Name[FT]): Operator[FT]
    def binaryExpression(operator: Operator[FT], left: any.Expression[FT], right: any.Expression[FT]): BinaryExpression[FT]
    def unaryExpression(operator: Operator[FT], operand: any.Expression[FT]): UnaryExpression[FT]

    implicit def convert(operator: Operator[FT]): Operator[FT]
    implicit def convert(binaryExpression: BinaryExpression[FT]): BinaryExpression[FT]
    implicit def convert(unaryExpression: UnaryExpression[FT]): UnaryExpression[FT]
  }
}