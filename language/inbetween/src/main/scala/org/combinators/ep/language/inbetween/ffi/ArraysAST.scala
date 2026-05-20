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

      type ArrayExpression <: arraysOps.ArrayExpression
      type LengthArrayExpression <: arraysOps.LengthArrayExpression
      type SetArrayExpression <: arraysOps.SetArrayExpression
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

    // expect usage is for gets and length
    trait ArrayExpression extends any.Expression {
      def getSelfArrayExpression: arraysOpsFinalTypes.ArrayExpression

      def base: any.Expression
      def indices: Seq[any.Expression]

      def copy(
                base: any.Expression = base,
                indices: Seq[any.Expression] = indices
              ): ArrayExpression = arraysOpsFactory.arrayExpression(base, indices)
    }

    // this includes value for set
    trait SetArrayExpression extends any.Expression {
      def getSelfSetArrayExpression: arraysOpsFinalTypes.SetArrayExpression
      def base: any.Expression
      def indices: Seq[any.Expression]
      def value: any.Expression

      def copy(
                base: any.Expression = base,
                indices: Seq[any.Expression] = indices,
                value: any.Expression = value
              ): SetArrayExpression = arraysOpsFactory.setArrayExpression(base, indices, value)
    }

//    trait GetArrayOp extends operatorExpressions.Operator
//
//    trait SetArrayOp extends operatorExpressions.Operator
//
    trait LengthArrayExpression  extends any.Expression {
      def getSelfLengthArrayExpression: arraysOpsFinalTypes.LengthArrayExpression
        def base: any.Expression
        def indices: Seq[any.Expression]
    
        def copy(
                base: any.Expression = base,
                indices: Seq[any.Expression] = indices
              ): LengthArrayExpression = arraysOpsFactory.lengthArrayExpression(base, indices)
    }

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

      def arrayExpression(base: any.Expression, indices: Seq[any.Expression]): ArrayExpression
      def setArrayExpression(base: any.Expression, indices: Seq[any.Expression], value: any.Expression): SetArrayExpression
      def lengthArrayExpression(base: any.Expression, indices: Seq[any.Expression]): LengthArrayExpression

//      def getArrayOp(): GetArrayOp
//      def setArrayOp(): SetArrayOp
//      def lengthArrayOp(): LengthArrayOp

      def getArrayOp(ar: any.Expression, indices: Seq[any.Expression]): arraysOpsFinalTypes.ArrayExpression =
        arraysOpsFactory.arrayExpression(ar, indices)
      def setArrayOp(ar: any.Expression, indices: Seq[any.Expression], value: any.Expression): arraysOpsFinalTypes.SetArrayExpression =
        arraysOpsFactory.setArrayExpression(ar, indices, value)
      def lengthArrayOp(ar: any.Expression, indices: Seq[any.Expression]): arraysOpsFinalTypes.LengthArrayExpression =
        arraysOpsFactory.lengthArrayExpression(ar, indices)
        
      implicit def convert(other: CreateArray): arraysOpsFinalTypes.CreateArrayExpression = other.getSelfCreateArrayExpression
      implicit def convert(other: CreateArrayFromExpression): arraysOpsFinalTypes.CreateArrayFromExpression = other.getSelfCreateArrayFromExpression
      implicit def convert(other: CreateArrayFromValues): arraysOpsFinalTypes.CreateArrayFromValues = other.getSelfCreateArrayFromValues
      implicit def convert(other: CreateArrayWithDefaultValues): arraysOpsFinalTypes.CreateArrayWithDefaultValues = other.getSelfCreateArrayWithDefaultValues
      implicit def convert(other: ArrayExpression): arraysOpsFinalTypes.ArrayExpression = other.getSelfArrayExpression
      implicit def convert(other: SetArrayExpression): arraysOpsFinalTypes.SetArrayExpression = other.getSelfSetArrayExpression
      implicit def convert(other: LengthArrayExpression): arraysOpsFinalTypes.LengthArrayExpression = other.getSelfLengthArrayExpression
    }
  }

  val arraysOpsFinalTypes: arraysOps.FinalTypes
  val arraysOpsFactory: arraysOps.Factory
}
