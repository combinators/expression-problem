package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait ArraysAST extends OperatorExpressionOpsAST with ParametricPolymorphismAST {
  object arraysOps {
    trait FinalTypes {
      type Array <: arraysOps.Array
      type CreateArrayExpression <: arraysOps.CreateArray
    }

    trait Array extends any.Type {
      def getSelfArrayType: arraysOpsFinalTypes.Array
    }

    trait CreateArray extends any.Expression {
      def getSelfCreateArrayExpression: arraysOpsFinalTypes.Array

      def elementType: any.Type
      def dimension: any.Expression
      def initialization: Either[any.Expression, Seq[any.Expression]]

      def copy(elementType: any.Type, dimension: any.Expression, initialization: Either[any.Expression, Seq[any.Expression]]) =
        arraysOpsFactory.createArray(elementType, dimension, initialization)
    }

    trait GetArrayOp extends operatorExpressions.Operator

    trait SetArrayOp extends operatorExpressions.Operator

    trait LengthArrayOp extends operatorExpressions.Operator

    trait Factory {

      def array(): arraysOps.Array

      def array(elementType: any.Type): any.Type = {
        polymorphismFactory.typeApplication(array(), Seq(elementType))
      }

      def createArray(elementType: any.Type, dimension: any.Expression, initialization: Either[any.Expression, Seq[any.Expression]]): CreateArray

      def createArray(tpe: any.Type, dimensions: Seq[any.Expression], contentSpec: Option[(Seq[Int], Seq[any.Expression])]): any.Expression = {
        contentSpec match {
          case Some((dims, values)) =>
            val initializers = dimensions.zip(dims).reverse.tail.foldLeft[(any.Type, Seq[any.Expression])]({
              val arrayTpe = array(tpe)
              val arrayExpr = values.grouped(dims.last).toSeq.map(subSeq =>
                arraysOpsFactory.createArray(tpe, dimensions.last, Right(subSeq)))
              (arrayTpe, arrayExpr)
            }) { case ((arrayTpe, inits), (dimension, dim)) =>
              val outerArrayTpe = arraysOpsFactory.array(arrayTpe)
              val outerArrayExpr = inits.grouped(dim).toSeq.map(subSeq => 
                arraysOpsFactory.createArray(arrayTpe, dimension, Right(subSeq)))
              (outerArrayTpe, outerArrayExpr)
            }

            initializers._2.head

          case None =>
            val (_, arrayExpr) = dimensions.reverse.tail.foldLeft[(any.Type, arraysOps.CreateArray)]({
              val arrayTpe = arraysOpsFactory.array(tpe)
              val arrayExpr = arraysOpsFactory.createArray(tpe, dimensions.last, Left(null)) // TODO: null needs to be a default expression of type tpe
              (arrayTpe, arrayExpr)
            }) { case ((arrayTpe, innerInitializer), dimension) =>
              val outerArrayTpe = arraysOpsFactory.array(arrayTpe)
              val outerArrayExpr = arraysOpsFactory.createArray(arrayTpe, dimension, Left(innerInitializer))
              (outerArrayTpe, outerArrayExpr)
            }
            arrayExpr
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
    }
  }

  val arraysOpsFinalTypes: arraysOps.FinalTypes
  val arraysOpsFactory: arraysOps.Factory
}
