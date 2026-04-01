package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{ArraysAST => InbetweenArraysAST}
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}

trait ArraysAST extends InbetweenArraysAST {
  self: OperatorExpressionsAST & BaseAST =>
  object scalaArraysOps {
    object arraysOpsOverride {

      trait CreateArray extends arraysOps.CreateArray with scalaBase.anyOverrides.Type {
        override def toScala: String = "Array"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): CreateArray =
          this

        def toImport: Seq[any.Import] = Seq.empty
      }

      trait GetArrayOp extends arraysOps.GetArrayOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
        def operator: String = ".apply"

        import factory._

        override def toScala(operands: any.Expression*): String = {
          s"${operands(0).toScala}(${operands(1).toScala})"
        }
      }

      trait SetArrayOp extends arraysOps.SetArrayOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
        def operator: String = ".apply"

        import factory._

        override def toScala(operands: any.Expression*): String = {
          s"${operands(0).toScala}(${operands(1).toScala}) = ${operands(2).toScala}"
        }
      }

      trait LengthArrayOp extends arraysOps.LengthArrayOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
        def operator: String = ".length"
      }

      trait Factory extends arraysOps.Factory {}
    }
  }

  val arraysOpsFactory: scalaArraysOps.arraysOpsOverride.Factory
}

trait FinalArraysAST extends ArraysAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>
  object finalArraysFactoryTypes {
    trait FinalArraysFactory extends scalaArraysOps.arraysOpsOverride.Factory {
      def createArray(dimension:any.Expression): arraysOps.CreateArray = {
        case class CreateArray(dimension:any.Expression) extends scalaArraysOps.arraysOpsOverride.CreateArray {
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        CreateArray(dimension)
      }

      def getArrayOp(): arraysOps.GetArrayOp = {
        case class GetArrayOp() extends scalaArraysOps.arraysOpsOverride.GetArrayOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        GetArrayOp()
      }

      def setArrayOp(): arraysOps.SetArrayOp = {
        case class SetArrayOp() extends scalaArraysOps.arraysOpsOverride.SetArrayOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        SetArrayOp()
      }

      def lengthArrayOp(): arraysOps.LengthArrayOp = {
        case class LengthArrayOp() extends scalaArraysOps.arraysOpsOverride.LengthArrayOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        LengthArrayOp()
      }
    }
  }

  val arraysOpsFactory: finalArraysFactoryTypes.FinalArraysFactory = new finalArraysFactoryTypes.FinalArraysFactory {}
}
