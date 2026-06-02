package org.combinators.ep.language.scala.ast.ffi     /*DI:LD:AI*/

import org.combinators.ep.language.inbetween.ffi.StringAST as InbetweenStringsAST
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait StringAST extends InbetweenStringsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaStringOps {
    object stringOpsOverride {

      trait AppendStringOp extends stringOps.AppendStringOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "++"
      }

      trait GetCharAtOp extends stringOps.GetCharAtOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = s".charAt"
      }
      
      trait StringLengthOp extends stringOps.StringLengthOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.PostfixOperator {
        override def operator: String = ".length"
      }

      trait SubStringOp extends stringOps.SubStringOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.PostfixOperator {
        override def operator: String = {
          ".substring"
        }

        override def toScala(operands: any.Expression*): String = {
          val base = operands.head.getSelfExpression.toScala
          val ops = operands.tail.map(arg => arg.getSelfExpression.toScala).mkString(",")
          s"${base}.substring($ops)"
        }
      }

      trait ToStringOp extends stringOps.ToStringOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.PostfixOperator {
        override def operator: String = ".toString()"
      }

      trait Factory extends stringOps.Factory {}
    }
  }

  override val stringOpsFactory: scalaStringOps.stringOpsOverride.Factory
}

trait FinalStringAST extends StringAST { self: FinalOperatorExpressionsAST & BaseAST =>
  object finalStringsFactoryTypes {
    trait FinalStringsFactory extends scalaStringOps.stringOpsOverride.Factory {
      def appendStringOp(): stringOps.AppendStringOp = {
        case class AppendStringOp() extends scalaStringOps.stringOpsOverride.AppendStringOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        AppendStringOp()
      }
      def getCharAtOp(): stringOps.GetCharAtOp = {
        case class GetCharAtOp() extends scalaStringOps.stringOpsOverride.GetCharAtOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        GetCharAtOp()
      }
      def stringLengthOp(): stringOps.StringLengthOp = {
        case class StringLengthOp() extends scalaStringOps.stringOpsOverride.StringLengthOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        StringLengthOp()
      }
      def subStringOp(): stringOps.SubStringOp = {
        case class SubStringOp() extends scalaStringOps.stringOpsOverride.SubStringOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        SubStringOp()
      }
      def toStringOp(): stringOps.ToStringOp = {
        case class ToStringOp() extends scalaStringOps.stringOpsOverride.ToStringOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        ToStringOp()
      }
    }
  }
  
  override val stringOpsFactory: finalStringsFactoryTypes.FinalStringsFactory = new finalStringsFactoryTypes.FinalStringsFactory {}
}
