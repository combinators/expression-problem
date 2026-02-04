package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{BooleanAST => InbetweenBooleanAST}
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait BooleanAST extends InbetweenBooleanAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaBooleanOps {
    object booleanOpsOverride {

      trait AndOp
        extends booleanOps.AndOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "&&"
      }
      trait OrOp extends booleanOps.OrOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.InfixOperator {
        override def operator: String = "||"
      }
      trait NotOp extends booleanOps.NotOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.PrefixOperator {
        override def operator: String = "!"
      }
      trait True extends booleanOps.True with scalaBase.anyOverrides.Expression {
        def toScala: String = "true"
        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Expression = this
      }
      trait False extends booleanOps.False with scalaBase.anyOverrides.Expression {
        def toScala: String = "false"
        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Expression = this
      }
      
      trait Factory extends booleanOps.Factory {}
    }
  }

  override val booleanOpsFactory: scalaBooleanOps.booleanOpsOverride.Factory
}

trait FinalBooleanAST extends BooleanAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>
  object finalBooleanFactoryTypes {
    trait FinalBooleanFactory extends scalaBooleanOps.booleanOpsOverride.Factory {
      def andOp(): booleanOps.AndOp = {
        case class AndOp() extends scalaBooleanOps.booleanOpsOverride.AndOp 
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        AndOp()
      }
      def orOp(): booleanOps.OrOp = {
        case class OrOp() extends scalaBooleanOps.booleanOpsOverride.OrOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        OrOp()
      }
      def notOp(): booleanOps.NotOp = {
        case class NotOp() extends scalaBooleanOps.booleanOpsOverride.NotOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        NotOp()
      }
      def trueExp(): booleanOps.True = {
        case class True() extends scalaBooleanOps.booleanOpsOverride.True
          with finalBaseAST.anyOverrides.FinalExpression {}
        True()
      }
      def falseExp(): booleanOps.False = {
        case class False() extends scalaBooleanOps.booleanOpsOverride.False
          with finalBaseAST.anyOverrides.FinalExpression {}
        False()
      }
    }
  }
  
  val booleanOpsFactory: finalBooleanFactoryTypes.FinalBooleanFactory = new finalBooleanFactoryTypes.FinalBooleanFactory {}
}
