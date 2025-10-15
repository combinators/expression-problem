package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyAST

trait OperatorExpressionOpsAST extends AnyAST {
  object operatorExpressions {
    trait FinalTypes {
      type Operator <: operatorExpressions.Operator
      type BinaryExpression <: operatorExpressions.BinaryExpression
      type UnaryExpression <: operatorExpressions.UnaryExpression
    }

    trait BinaryExpression extends any.Expression {
      def getSelfBinaryExpression: operatorExpressionsFinalTypes.BinaryExpression
      def operator: Operator
      def left: any.Expression
      def right: any.Expression

      def copy(
        operator: Operator = operator,
        left: any.Expression = left,
        right: any.Expression = right
      ): BinaryExpression = operatorExpressionsFactory.binaryExpression(operator, left, right)
    }

    trait UnaryExpression extends any.Expression {
      def getSelfUnaryExpression: operatorExpressionsFinalTypes.UnaryExpression
      def operator: Operator
      def operand: any.Expression

      def copy(
        operator: Operator = operator,
        operand: any.Expression = operand
      ): UnaryExpression = operatorExpressionsFactory.unaryExpression(operator, operand)
    }

    trait Operator extends Factory {
      def getSelfOperator: operatorExpressionsFinalTypes.Operator
    }

    trait Factory {
      def binaryExpression(operator: Operator, left: any.Expression, right: any.Expression): BinaryExpression
      def unaryExpression(operator: Operator, operand: any.Expression): UnaryExpression

      implicit def convert(other: Operator): operatorExpressionsFinalTypes.Operator = other.getSelfOperator
      implicit def convert(other: BinaryExpression): operatorExpressionsFinalTypes.BinaryExpression = other.getSelfBinaryExpression
      implicit def convert(other: UnaryExpression): operatorExpressionsFinalTypes.UnaryExpression = other.getSelfUnaryExpression
    }
  }
  
  val operatorExpressionsFinalTypes: operatorExpressions.FinalTypes
  val operatorExpressionsFactory: operatorExpressions.Factory
}