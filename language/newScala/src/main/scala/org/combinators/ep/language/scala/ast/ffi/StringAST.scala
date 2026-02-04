package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.StringAST as InbetweenStringsAST
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait StringAST extends InbetweenStringsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaStringOps {
    object stringOpsOverride {

      trait ToStringOp extends stringOps.ToStringOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.PostfixOperator {
        override def operator: String = ".toString()"
      }

      trait AppendStringOp extends stringOps.AppendStringOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "++"
      }

      trait StringLengthOp extends stringOps.StringLengthOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.PostfixOperator {
        override def operator: String = ".length"
      }
      
      trait Factory extends stringOps.Factory {}
    }
  }

  override val stringOpsFactory: scalaStringOps.stringOpsOverride.Factory
}

trait FinalStringAST extends StringAST { self: FinalOperatorExpressionsAST & BaseAST =>
  object finalStringsFactoryTypes {
    trait FinalStringsFactory extends scalaStringOps.stringOpsOverride.Factory {
      def toStringOp(): stringOps.ToStringOp = {
        case class ToStringOp() extends scalaStringOps.stringOpsOverride.ToStringOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        ToStringOp()
      }
      def appendStringOp(): stringOps.AppendStringOp = {
        case class AppendStringOp() extends scalaStringOps.stringOpsOverride.AppendStringOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        AppendStringOp()
      }
      def stringLengthOp(): stringOps.StringLengthOp = {
        case class StringLengthOp() extends scalaStringOps.stringOpsOverride.StringLengthOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        StringLengthOp()
      }
    }
  }
  
  override val stringOpsFactory: finalStringsFactoryTypes.FinalStringsFactory = new finalStringsFactoryTypes.FinalStringsFactory {}
}
