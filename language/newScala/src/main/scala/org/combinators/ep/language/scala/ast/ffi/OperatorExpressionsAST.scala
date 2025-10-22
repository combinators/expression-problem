package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.OperatorExpressionOpsAST as InbetweenOperatorExpressionOpsAST
import org.combinators.ep.language.scala.ast.BaseAST

trait OperatorExpressionsAST extends InbetweenOperatorExpressionOpsAST{ self: BaseAST =>
  object scalaOperatorExpressions {
    object operatorExpressionsOverrides {
      trait FinalTypes extends operatorExpressions.FinalTypes {
        type Operator <: operatorExpressionsOverrides.Operator
        type BinaryExpression <: operatorExpressionsOverrides.BinaryExpression
        type UnaryExpression <: operatorExpressionsOverrides.UnaryExpression
      }

      trait BinaryExpression extends scalaBase.anyOverrides.Expression with operatorExpressions.BinaryExpression {
        import operatorExpressionsFactory.*
        import factory.*
        def toScala: String = s"(${operator.toScala(left, right)})" // necessary when composing expressions, though can get excessive at times.

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): operatorExpressions.BinaryExpression =
          copy(
            left = left.prefixRootPackage(rootPackageName, excludedTypeNames),
            right = right.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait UnaryExpression extends scalaBase.anyOverrides.Expression with operatorExpressions.UnaryExpression {
        import operatorExpressionsFactory.*
        import factory.*
        def toScala: String = operator.toScala(operand)

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): operatorExpressions.UnaryExpression =
          copy(
            operand = operand.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait Operator extends operatorExpressions.Operator {
        def toScala(operands: any.Expression*): String
      }
      
      trait Factory extends operatorExpressions.Factory {}
    }
  }

  val operatorExpressionsFinalTypes: scalaOperatorExpressions.operatorExpressionsOverrides.FinalTypes
  val operatorExpressionsFactory: scalaOperatorExpressions.operatorExpressionsOverrides.Factory
}
