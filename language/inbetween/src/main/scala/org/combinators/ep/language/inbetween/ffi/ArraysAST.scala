package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait ArraysAST extends OperatorExpressionOpsAST with ParametricPolymorphismAST {
  object arraysOps {
    trait FinalTypes {
      type Array <: arraysOps.Array
      type CreateArrayExpression <: arraysOps.CreateArray
      type CreateArrayFromExpression <: arraysOps.CreateArrayFromExpression
      type CreateArrayWithDefaultValues <: arraysOps.CreateArrayWithDefaultValues
      type CreateArrayFromValues <: arraysOps.CreateArrayFromValues
    }

    trait Array extends any.Type {
      def getSelfArrayType: arraysOpsFinalTypes.Array
    }

    trait CreateArray extends any.Expression {
      def getSelfCreateArrayExpression: arraysOpsFinalTypes.CreateArrayExpression
    }

    trait CreateArrayFromExpression extends CreateArray {
      def getSelfCreateArrayFromExpression: arraysOpsFinalTypes.CreateArrayFromExpression
      def expression: any.Expression
      def copy(expression: any.Expression = this.expression): CreateArrayFromExpression =
        arraysOpsFactory.createArrayFromExpression(expression)
    }

    trait CreateArrayWithDefaultValues extends CreateArray {
      def getSelfCreateArrayWithDefaultValues: arraysOpsFinalTypes.CreateArrayWithDefaultValues
      def tpe: any.Type
      def dimensions: Seq[any.Expression]
      def copy(tpe: any.Type = this.tpe, dimensions: Seq[any.Expression] = this.dimensions): CreateArrayWithDefaultValues =
        arraysOpsFactory.createArrayWithDefaultValues(tpe, dimensions)
    }

    trait CreateArrayFromValues extends CreateArray {
      def getSelfCreateArrayFromValues: arraysOpsFinalTypes.CreateArrayFromValues
      def values: Seq[any.Expression]
      def copy(values: Seq[any.Expression] = this.values): CreateArrayFromValues =
        arraysOpsFactory.createArrayFromValues(values)
    }

    trait GetArrayOp extends operatorExpressions.Operator

    trait SetArrayOp extends operatorExpressions.Operator

    trait LengthArrayOp extends operatorExpressions.Operator

    trait Factory {

      def array(): arraysOps.Array

      def array(elementType: any.Type): any.Type = {
        polymorphismFactory.typeApplication(array(), Seq(elementType))
      }

      def createArrayFromExpression(expression: any.Expression): CreateArrayFromExpression

      def createArrayWithDefaultValues(tpe: any.Type, dimensions: Seq[any.Expression]): CreateArrayWithDefaultValues

      def createArrayFromValues(values: Seq[any.Expression]): CreateArrayFromValues

      def createArray(tpe: any.Type, dimensions: Seq[any.Expression], contentSpec: Option[(Seq[Int], Seq[any.Expression])]): any.Expression = {
        contentSpec match {
          case Some((dims, values)) =>
            val initializers = dimensions.zip(dims).reverse.tail.foldLeft[(any.Type, Seq[any.Expression])]({
              val arrayTpe = array(tpe)
              val arrayExpr = values.grouped(dims.last).toSeq.map(subSeq =>
                arraysOpsFactory.createArrayFromValues(subSeq))
              (arrayTpe, arrayExpr)
            }) { case ((arrayTpe, inits), (dimension, dim)) =>
              val outerArrayTpe = arraysOpsFactory.array(arrayTpe)
              val outerArrayExpr = inits.grouped(dim).toSeq.map(subSeq =>
                arraysOpsFactory.createArrayFromValues(subSeq))
              (outerArrayTpe, outerArrayExpr)
            }

            initializers._2.head

          case None => createArrayWithDefaultValues(tpe, dimensions)
        }
      }

      def getArrayOp(): GetArrayOp
      def setArrayOp(): SetArrayOp
      def lengthArrayOp(): LengthArrayOp

      def getArrayOp(ar: any.Expression, idx: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(getArrayOp(), ar, idx)
      def setArrayOp(ar: any.Expression, idx: any.Expression, value: any.Expression): operatorExpressions.TernaryExpression =
        operatorExpressionsFactory.ternaryExpression(getArrayOp(), ar, idx, value)
      def lengthArrayOp(ar: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(lengthArrayOp(), ar)
        
      implicit def convert(other: CreateArray): arraysOpsFinalTypes.CreateArrayExpression = other.getSelfCreateArrayExpression
      implicit def convert(other: CreateArrayFromExpression): arraysOpsFinalTypes.CreateArrayFromExpression = other.getSelfCreateArrayFromExpression
      implicit def convert(other: CreateArrayFromValues): arraysOpsFinalTypes.CreateArrayFromValues = other.getSelfCreateArrayFromValues
      implicit def convert(other: CreateArrayWithDefaultValues): arraysOpsFinalTypes.CreateArrayWithDefaultValues = other.getSelfCreateArrayWithDefaultValues
    }
  }

  val arraysOpsFinalTypes: arraysOps.FinalTypes
  val arraysOpsFactory: arraysOps.Factory
}
