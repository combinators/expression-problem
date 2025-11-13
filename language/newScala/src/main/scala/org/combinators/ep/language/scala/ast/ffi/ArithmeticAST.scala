package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{ArithmeticOpsAST => InbetweenArithmeticOpsAST}
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait ArithmeticAST extends InbetweenArithmeticOpsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaArithmeticOps {
    object arithmeticOpsOverride {

      trait AddOp
        extends arithmeticOps.AddOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "+"
      }
      trait SubOp extends arithmeticOps.SubOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "-"
      }
      trait MultOp extends arithmeticOps.MultOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "*"
      }
      trait DivOp extends arithmeticOps.DivOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "/"
      }
      trait ModOp extends arithmeticOps.ModOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "%"
      }
      trait LtOp extends arithmeticOps.LtOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "<"
      }
      trait LeOp extends arithmeticOps.LeOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "<="
      }
      
      trait Factory extends arithmeticOps.Factory {}
    }
  }
  val arithmeticOpsFactory: scalaArithmeticOps.arithmeticOpsOverride.Factory
}

trait FinalArithmeticAST extends ArithmeticAST { self: FinalOperatorExpressionsAST & BaseAST =>
  object finalArithmeticFactoryTypes {
    trait FinalArithmeticFactory extends scalaArithmeticOps.arithmeticOpsOverride.Factory {
      def addOp(): arithmeticOps.AddOp = {
        case class AddOp() extends scalaArithmeticOps.arithmeticOpsOverride.AddOp 
          with finalOperatorExpressions .operatorExpressionsOverrides.Operator {}
        AddOp()
      }
      def subOp(): arithmeticOps.SubOp = {
        case class SubOp() extends scalaArithmeticOps.arithmeticOpsOverride.SubOp
          with finalOperatorExpressions .operatorExpressionsOverrides.Operator {}
        SubOp()
      }
      def multOp(): arithmeticOps.MultOp = {
        case class MultOp() extends scalaArithmeticOps.arithmeticOpsOverride.MultOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        MultOp()
      }
      def divOp(): arithmeticOps.DivOp = {
        case class DivOp() extends scalaArithmeticOps.arithmeticOpsOverride.DivOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        DivOp()
      }
      def modOp(): arithmeticOps.ModOp = {
        case class ModOp() extends scalaArithmeticOps.arithmeticOpsOverride.ModOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        ModOp()
      }
      def ltOp(): arithmeticOps.LtOp = {
        case class LtOp() extends scalaArithmeticOps.arithmeticOpsOverride.LtOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        LtOp()
      }
      def leOp(): arithmeticOps.LeOp = {
        case class LeOp() extends scalaArithmeticOps.arithmeticOpsOverride.LeOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        LeOp()
      }
    }
  }
  
  val arithmeticOpsFactory: finalArithmeticFactoryTypes.FinalArithmeticFactory = new finalArithmeticFactoryTypes.FinalArithmeticFactory {}
}
