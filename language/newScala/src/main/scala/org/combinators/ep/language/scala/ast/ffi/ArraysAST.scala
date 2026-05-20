package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{ArraysAST => InbetweenArraysAST}
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}

trait ArraysAST extends InbetweenArraysAST {
  self: OperatorExpressionsAST & BaseAST =>
  object scalaArraysOps {
    object arraysOpsOverride {
      trait FinalTypes extends arraysOps.FinalTypes {
        type Array <: arraysOpsOverride.Array
        type CreateArrayExpression <: arraysOpsOverride.CreateArray
        type CreateArrayFromExpression <: arraysOpsOverride.CreateArrayFromExpression
        type CreateArrayWithDefaultValues <: arraysOpsOverride.CreateArrayWithDefaultValues
        type CreateArrayFromValues <: arraysOpsOverride.CreateArrayFromValues
        type ArrayExpression <: arraysOpsOverride.ArrayExpression
        type LengthArrayExpression <: arraysOpsOverride.LengthArrayExpression
        type SetArrayExpression <: arraysOpsOverride.SetArrayExpression
      }

      trait Array extends arraysOps.Array with scalaBase.anyOverrides.Type {
        override def toScala: String = "Array"
        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): Array =
          this
        override def toImport: Seq[any.Import] = Seq.empty
      }
      
      trait CreateArray extends arraysOps.CreateArray with scalaBase.anyOverrides.Expression {
        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): CreateArray
      }
      
      trait CreateArrayFromExpression extends arraysOps.CreateArrayFromExpression with CreateArray {
        import factory._
        import arraysOpsFactory._
        override def toScala: String = expression.toScala

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): CreateArrayFromExpression =
          arraysOpsFactory.convert(copy(expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)))
        
        override def toImport: Seq[any.Import] = Seq.empty
      }
      
      trait CreateArrayWithDefaultValues extends arraysOps.CreateArrayWithDefaultValues with CreateArray {
        import factory._
        import arraysOpsFactory.convert
        override def toScala: String = s"Array.ofDim[${tpe.toScala}](${dimensions.map(_.toScala).mkString(", ")})"
        override def prefixRootPackage(rootPackageName:  scala.Seq[ArraysAST.this.any.Name], excludedTypeNames:  _root_.scala.Predef.Set[scala.Seq[ArraysAST.this.any.Name]]): CreateArrayWithDefaultValues =
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            dimensions = dimensions.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
      }

      trait CreateArrayFromValues extends arraysOps.CreateArrayFromValues with CreateArray {
        import factory._
        import arraysOpsFactory.convert

        override def toScala: String = s"Array(${values.map(_.toScala).mkString(", ")})"
        override def prefixRootPackage(rootPackageName: scala.Seq[ArraysAST.this.any.Name], excludedTypeNames: _root_.scala.Predef.Set[scala.Seq[ArraysAST.this.any.Name]]): CreateArrayFromValues =
          copy(
            values = values.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
      }

      trait ArrayExpression extends arraysOps.ArrayExpression
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.PostfixOperator
        with scalaBase.anyOverrides.Expression {
// LengthArrayOp extends arraysOps.LengthArrayOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
        //
        import factory._
        import arraysOpsFactory.convert

        override def toScala: String = {
          val indexPairs = indices.map(idx => s"(${idx.toScala})").mkString("")
          s"${base.toScala}$indexPairs"
        }

        override def prefixRootPackage(rootPackageName: scala.Seq[ArraysAST.this.any.Name], excludedTypeNames: _root_.scala.Predef.Set[scala.Seq[ArraysAST.this.any.Name]]): ArrayExpression =
          copy(
            base = base.prefixRootPackage(rootPackageName, excludedTypeNames),
            indices = indices
          )
      }

      trait SetArrayExpression extends arraysOps.SetArrayExpression with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaBase.anyOverrides.Statement {

        import factory._
        import arraysOpsFactory.convert

        override def toScala: String = {
          val indexPairs = indices.map(idx => s"(${idx.toScala})").mkString("")
          s"${base.toScala}${indexPairs} = ${value.toScala}"
        }

        override def prefixRootPackage(rootPackageName: scala.Seq[ArraysAST.this.any.Name], excludedTypeNames: _root_.scala.Predef.Set[scala.Seq[ArraysAST.this.any.Name]]): SetArrayExpression =
          copy(
            base = base.prefixRootPackage(rootPackageName, excludedTypeNames),
            indices = indices,
            value = value
          )
      }

//      trait GetArrayOp extends arraysOps.GetArrayOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
//        def operator: String = ".apply"
//
//        import factory._
//
//        override def toScala(operands: any.Expression*): String = {
//          s"${operands(0).toScala}(${operands(1).toScala})"
//        }
//      }
//
//      trait SetArrayOp extends arraysOps.SetArrayOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
//        def operator: String = ".apply"
//
//        import factory._
//
//        override def toScala(operands: any.Expression*): String = {
//          s"${operands(0).toScala}(${operands(1).toScala}) = ${operands(2).toScala}"
//        }
//      }
//
      trait LengthArrayExpression extends arraysOps.LengthArrayExpression
            with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
            with scalaOperatorExpressions.PostfixOperator
            with scalaBase.anyOverrides.Expression
         {
        import factory._
        import arraysOpsFactory.convert

        override def toScala: String = {
          if (indices.nonEmpty) {
            val indexPairs = indices.map(idx => s"(${idx.toScala})").mkString("")
            s"${base.toScala}$indexPairs.length"
          } else {
            s"${base.toScala}.length"
          }
        }

        override def prefixRootPackage(rootPackageName: scala.Seq[ArraysAST.this.any.Name], excludedTypeNames: _root_.scala.Predef.Set[scala.Seq[ArraysAST.this.any.Name]]): LengthArrayExpression =
          copy(
            base = base.prefixRootPackage(rootPackageName, excludedTypeNames),
            indices = indices
          )
      }

      trait Factory extends arraysOps.Factory {}
    }
  }

  override val arraysOpsFinalTypes: scalaArraysOps.arraysOpsOverride.FinalTypes
  override val arraysOpsFactory: scalaArraysOps.arraysOpsOverride.Factory
}

trait FinalArraysAST extends ArraysAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>
  object finalArraysFinalTypes {
    trait ArraysFinalTypes extends scalaArraysOps.arraysOpsOverride.FinalTypes {
      type Array = scalaArraysOps.arraysOpsOverride.Array
      type CreateArrayExpression = scalaArraysOps.arraysOpsOverride.CreateArray
      type CreateArrayFromExpression = scalaArraysOps.arraysOpsOverride.CreateArrayFromExpression
      type CreateArrayWithDefaultValues = scalaArraysOps.arraysOpsOverride.CreateArrayWithDefaultValues
      type CreateArrayFromValues = scalaArraysOps.arraysOpsOverride.CreateArrayFromValues
      type ArrayExpression = scalaArraysOps.arraysOpsOverride.ArrayExpression
      type SetArrayExpression = scalaArraysOps.arraysOpsOverride.SetArrayExpression
      type LengthArrayExpression = scalaArraysOps.arraysOpsOverride.LengthArrayExpression
    }
  }
  override val arraysOpsFinalTypes: finalArraysFinalTypes.ArraysFinalTypes = new finalArraysFinalTypes.ArraysFinalTypes {}

  object finalArraysFactoryTypes {
    trait FinalArraysFactory extends scalaArraysOps.arraysOpsOverride.Factory {
      def array(): arraysOps.Array = {
        case class Array()
          extends scalaArraysOps.arraysOpsOverride.Array {
          override def getSelfArrayType: scalaArraysOps.arraysOpsOverride.Array = this
          override def getSelfType: scalaBase.anyOverrides.Type = this
        }
        Array()
      }

      def createArrayFromExpression(expression: any.Expression): scalaArraysOps.arraysOpsOverride.CreateArrayFromExpression = {
        case class CreateArrayFromExpression(val expression: any.Expression)
          extends scalaArraysOps.arraysOpsOverride.CreateArrayFromExpression
            with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfCreateArrayExpression: arraysOpsFinalTypes.CreateArrayExpression = this
          override def getSelfCreateArrayFromExpression: arraysOpsFinalTypes.CreateArrayFromExpression = this
        }
        CreateArrayFromExpression(expression)
      }

      def createArrayWithDefaultValues(tpe: any.Type, dimensions: Seq[any.Expression]): scalaArraysOps.arraysOpsOverride.CreateArrayWithDefaultValues = {
        case class CreateArrayWithDefaultValues(tpe: any.Type, dimensions: Seq[any.Expression])
            extends scalaArraysOps.arraysOpsOverride.CreateArrayWithDefaultValues
            with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfCreateArrayExpression: arraysOpsFinalTypes.CreateArrayExpression = this
          override def getSelfCreateArrayWithDefaultValues: arraysOpsFinalTypes.CreateArrayWithDefaultValues = this
        }
        CreateArrayWithDefaultValues(tpe, dimensions)
      }

      def createArrayFromValues(values: Seq[any.Expression]): scalaArraysOps.arraysOpsOverride.CreateArrayFromValues = {
        case class CreateArrayFromValues(values: Seq[any.Expression])
          extends scalaArraysOps.arraysOpsOverride.CreateArrayFromValues
            with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfCreateArrayExpression: arraysOpsFinalTypes.CreateArrayExpression = this
          override def getSelfCreateArrayFromValues: arraysOpsFinalTypes.CreateArrayFromValues = this
        }
        CreateArrayFromValues(values)
      }
//
//      def getArrayOp(base: any.Expression, indices: Seq[any.Expression]): arraysOps.GetArrayOp = {
//        case class GetArrayOp(base: any.Expression, indices: Seq[any.Expression]) extends scalaArraysOps.arraysOpsOverride.GetArrayOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
//        GetArrayOp(base, indices)
//      }
//
//      def setArrayOp(base: any.Expression, indices: Seq[any.Expression], value: any.Expression): arraysOps.SetArrayOp = {
//        case class SetArrayOp(base: any.Expression, indices: Seq[any.Expression], value: any.Expression) extends scalaArraysOps.arraysOpsOverride.SetArrayOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
//        SetArrayOp(base, indices, value)
//      }

      def lengthArrayExpression(base: any.Expression, indices: Seq[any.Expression]): scalaArraysOps.arraysOpsOverride.LengthArrayExpression = {
        case class LengthArrayExpression(base: any.Expression, indices: Seq[any.Expression])
             extends scalaArraysOps.arraysOpsOverride.LengthArrayExpression
               with finalOperatorExpressions.operatorExpressionsOverrides.Operator
               with finalBaseAST.anyOverrides.FinalExpression {
          def operator: String = ".length"

          override def getSelfLengthArrayExpression: scalaArraysOps.arraysOpsOverride.LengthArrayExpression = this
          override def getSelfExpression: finalBaseAST.anyOverrides.FinalExpression = this
        }

        LengthArrayExpression(base, indices)
      }

      def arrayExpression(base: any.Expression, indices: Seq[any.Expression]): scalaArraysOps.arraysOpsOverride.ArrayExpression = {
        case class ArrayExpression(base: any.Expression, indices: Seq[any.Expression])
            extends scalaArraysOps.arraysOpsOverride.ArrayExpression
              with finalOperatorExpressions.operatorExpressionsOverrides.Operator
              with finalBaseAST.anyOverrides.FinalExpression {

          override def getSelfArrayExpression: scalaArraysOps.arraysOpsOverride.ArrayExpression = this
          override def getSelfExpression: finalBaseAST.anyOverrides.FinalExpression = this

          override def operator: String = ".apply"
        }
        ArrayExpression(base, indices)
      }

      def setArrayExpression(base: any.Expression, indices: Seq[any.Expression], value: any.Expression): scalaArraysOps.arraysOpsOverride.SetArrayExpression = {
        case class SetArrayExpression(base: any.Expression, indices: Seq[any.Expression], value: any.Expression)
            extends scalaArraysOps.arraysOpsOverride.SetArrayExpression
            with finalOperatorExpressions.operatorExpressionsOverrides.Operator
            with finalBaseAST.anyOverrides.FinalExpression {
          override def toScala(operands: any.Expression*): String = ???

          override def getSelfSetArrayExpression: scalaArraysOps.arraysOpsOverride.SetArrayExpression = this
          override def getSelfStatement: finalTypes.Statement = this

        }
        SetArrayExpression(base, indices, value)
      }
    }
  }

  val arraysOpsFactory: finalArraysFactoryTypes.FinalArraysFactory = new finalArraysFactoryTypes.FinalArraysFactory {}
}
