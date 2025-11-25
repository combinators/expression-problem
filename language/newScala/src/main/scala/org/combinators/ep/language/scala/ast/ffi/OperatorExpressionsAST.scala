package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{OperatorExpressionOpsAST => InbetweenOperatorExpressionOpsAST}
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}

trait OperatorExpressionsAST extends InbetweenOperatorExpressionOpsAST{ self: BaseAST =>
  object scalaOperatorExpressions {
    object operatorExpressionsOverrides {
      trait FinalTypes extends operatorExpressions.FinalTypes {
        type Operator <: operatorExpressionsOverrides.Operator
        type BinaryExpression <: operatorExpressionsOverrides.BinaryExpression
        type UnaryExpression <: operatorExpressionsOverrides.UnaryExpression
      }

      trait BinaryExpression extends scalaBase.anyOverrides.Expression with operatorExpressions.BinaryExpression {

        def toScala: String = s"(${operator.getSelfOperator.toScala(left, right)})" // necessary when composing expressions, though can get excessive at times.

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): operatorExpressions.BinaryExpression =
          copy(
            left = left.getSelfExpression.prefixRootPackage(rootPackageName, excludedTypeNames),
            right = right.getSelfExpression.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait UnaryExpression extends scalaBase.anyOverrides.Expression with operatorExpressions.UnaryExpression {
        def toScala: String = operator.getSelfOperator.toScala(operand)

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): operatorExpressions.UnaryExpression =
          copy(
            operand = operand.getSelfExpression.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait Operator extends operatorExpressions.Operator {
        def toScala(operands: any.Expression*): String
      }
      
      trait Factory extends operatorExpressions.Factory {}
    }

    trait InfixOperator {
      def operator: String
      def toScala(operands: any.Expression*): String = operands.map(_.getSelfExpression.toScala).mkString(operator)
    }

    trait PrefixOperator {
      def operator: String
      def toScala(operands: any.Expression*): String = s"($operator${operands.head.getSelfExpression.toScala})"
    }

    trait MathFunctionOperator {
      import factory.*
      def operator: String
      def toScala(operands: any.Expression*): String = {
        s"Math.$operator${operands.map(_.getSelfExpression.toScala).mkString("(", ", ", ")")}"
      }
    }

    trait PostfixOperator {
      import factory.*
      def operator: String
      def toScala(operands: any.Expression*): String = s"(${operands.head.getSelfExpression.toScala}$operator)"
    }
  }

  val operatorExpressionsFinalTypes: scalaOperatorExpressions.operatorExpressionsOverrides.FinalTypes
  val operatorExpressionsFactory: scalaOperatorExpressions.operatorExpressionsOverrides.Factory
}

trait FinalOperatorExpressionsAST extends OperatorExpressionsAST { self: FinalBaseAST =>

  object finalOperatorExpressions {
    object operatorExpressionsOverrides {
      trait Operator extends scalaOperatorExpressions.operatorExpressionsOverrides.Operator {
        def getSelfOperator: scalaOperatorExpressions.operatorExpressionsOverrides.Operator = this
      }
    }
  }

  object finalOperatorExpressionsFinalTypes {
    trait OperatorExpressionsFinalTypes extends scalaOperatorExpressions.operatorExpressionsOverrides.FinalTypes {
      type Operator = scalaOperatorExpressions.operatorExpressionsOverrides.Operator
      type BinaryExpression = scalaOperatorExpressions.operatorExpressionsOverrides.BinaryExpression
      type UnaryExpression = scalaOperatorExpressions.operatorExpressionsOverrides.UnaryExpression
    }
  }
  override val operatorExpressionsFinalTypes: finalOperatorExpressionsFinalTypes.OperatorExpressionsFinalTypes = new finalOperatorExpressionsFinalTypes.OperatorExpressionsFinalTypes {}

  object finalOperatorExpressionsFactoryTypes {
    trait OperatorExpressionsFactory extends scalaOperatorExpressions.operatorExpressionsOverrides.Factory {
      def binaryExpression(operator: operatorExpressions.Operator, left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression = {
        case class BinaryExpression(operator: operatorExpressions.Operator, left: any.Expression, right: any.Expression)
        extends scalaOperatorExpressions.operatorExpressionsOverrides.BinaryExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfBinaryExpression: scalaOperatorExpressions.operatorExpressionsOverrides.BinaryExpression = this
        }
        BinaryExpression(operator, left, right)
      }
      def unaryExpression(operator: operatorExpressions.Operator, operand: any.Expression): operatorExpressions.UnaryExpression = {
        case class UnaryExpression(operator: operatorExpressions.Operator, operand: any.Expression)
        extends scalaOperatorExpressions.operatorExpressionsOverrides.UnaryExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfUnaryExpression: scalaOperatorExpressions.operatorExpressionsOverrides.UnaryExpression = this
        }
        UnaryExpression(operator, operand)
      }
    }
  }
  override val operatorExpressionsFactory: scalaOperatorExpressions.operatorExpressionsOverrides.Factory = new finalOperatorExpressionsFactoryTypes.OperatorExpressionsFactory {}
}