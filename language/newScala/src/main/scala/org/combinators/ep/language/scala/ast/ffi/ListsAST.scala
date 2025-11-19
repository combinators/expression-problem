package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.ListsAST as InbetweenListsAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}

trait ListsAST extends InbetweenListsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaListsOps {
    object listsOpsOverride {

      trait CreateList extends listsOps.CreateList with scalaBase.anyOverrides.Type {
        override def toScala: String = "Seq"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): CreateList =
          this
        def toImport: Seq[any.Import] = Seq.empty
      }

      trait ConsListOp extends listsOps.ConsListOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.InfixOperator {
        def operator: String = "+:"
      }

      trait HeadListOp extends listsOps.HeadListOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
        def operator: String = ".head"
      }

      trait TailListOp  extends listsOps.TailListOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
        def operator: String = ".tail"
      }

      trait AppendListOp  extends listsOps.AppendListOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.InfixOperator {
        def operator: String = "++"
      }
      
      trait Factory extends listsOps.Factory {}
    }
  }

  override val listsOpsFactory: scalaListsOps.listsOpsOverride.Factory
}

trait FinalListsAST extends ListsAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>
  object finalListsFactoryTypes {
    trait FinalListsFactory extends scalaListsOps.listsOpsOverride.Factory {
      def createList(): listsOps.CreateList = {
        case class CreateList() extends scalaListsOps.listsOpsOverride.CreateList {
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        CreateList()
      }
      def consListOp(): listsOps.ConsListOp = {
        case class ConsListOp() extends scalaListsOps.listsOpsOverride.ConsListOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator 
        ConsListOp()
      }
      def headListOp(): listsOps.HeadListOp = {
        case class HeadListOp() extends scalaListsOps.listsOpsOverride.HeadListOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        HeadListOp()
      }
      def tailListOp(): listsOps.TailListOp = {
        case class TailListOp() extends scalaListsOps.listsOpsOverride.TailListOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        TailListOp()
      }
      def appendListOp(): listsOps.AppendListOp = {
        case class AppendListOp() extends scalaListsOps.listsOpsOverride.AppendListOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        AppendListOp()
      }
    }
  }
  
  val listsOpsFactory: finalListsFactoryTypes.FinalListsFactory = new finalListsFactoryTypes.FinalListsFactory {}
}
