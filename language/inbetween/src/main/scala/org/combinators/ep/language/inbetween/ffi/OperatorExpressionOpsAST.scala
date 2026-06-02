package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.any.AnyAST

trait OperatorExpressionOpsAST extends AnyAST {
  object operatorExpressions {
    trait FinalTypes {
      type Operator <: operatorExpressions.Operator
      type TernaryExpression <: operatorExpressions.TernaryExpression
      type BinaryExpression <: operatorExpressions.BinaryExpression
      type UnaryExpression <: operatorExpressions.UnaryExpression
    }
    
    trait TernaryExpression extends any.Expression {
      def getSelfTernaryExpression: operatorExpressionsFinalTypes.TernaryExpression
      def operator: Operator
      def left: any.Expression
      def mid: any.Expression
      def right: any.Expression
      def copy(
        operator: Operator = operator,
        left: any.Expression = left,
        mid: any.Expression = mid,
        right: any.Expression = right
      ): TernaryExpression = operatorExpressionsFactory.ternaryExpression(operator, left, mid, right)
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

    trait Operator {
      def getSelfOperator: operatorExpressionsFinalTypes.Operator
    }

    trait Factory {
      def ternaryExpression(operator: Operator, left: any.Expression, middle: any.Expression, right: any.Expression): TernaryExpression
      def binaryExpression(operator: Operator, left: any.Expression, right: any.Expression): BinaryExpression
      def unaryExpression(operator: Operator, operand: any.Expression): UnaryExpression

      implicit def convert(other: Operator): operatorExpressionsFinalTypes.Operator = other.getSelfOperator
      implicit def convert(other: TernaryExpression): operatorExpressionsFinalTypes.TernaryExpression = other.getSelfTernaryExpression
      implicit def convert(other: BinaryExpression): operatorExpressionsFinalTypes.BinaryExpression = other.getSelfBinaryExpression
      implicit def convert(other: UnaryExpression): operatorExpressionsFinalTypes.UnaryExpression = other.getSelfUnaryExpression

    }
  }
  
  val operatorExpressionsFinalTypes: operatorExpressions.FinalTypes
  val operatorExpressionsFactory: operatorExpressions.Factory
}