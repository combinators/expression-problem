package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait ArraysAST extends OperatorExpressionOpsAST with ParametricPolymorphismAST {
  object arraysOps {
    trait CreateArray extends any.Type {
      def dimension: any.Expression
    }

    trait GetArrayOp extends operatorExpressions.Operator
    trait SetArrayOp extends operatorExpressions.Operator
    trait LengthArrayOp extends operatorExpressions.Operator

    trait Factory {
      def createArray(dimension: any.Expression): CreateArray

      def createArray(tpe: any.Type, dimensions:Seq[any.Expression], contentSpec:Option[(Seq[Int], Seq[any.Expression])]): any.Expression = {
        contentSpec match {

          case Some((dims, values)) =>
              val initializers = dimensions.zip(dims).reverse.tail.foldLeft[(any.Type,Seq[any.Expression])] ( {
                  val arrayTpe  = polymorphismFactory.typeApplication(createArray(dimensions.last), Seq(tpe))
                  val arrayExpr = values.grouped(dims.last).toSeq.map(subSeq =>
                    factory.applyExpression(polymorphismFactory.typeReferenceExpression(arrayTpe), subSeq))
                  (arrayTpe, arrayExpr)
                }
                ) { case ((arrayTpe, inits), (dimension, dim)) =>

                  val outerArrayTpe = polymorphismFactory.typeApplication(createArray(dimension), Seq(arrayTpe))
                  val outerArrayExpr = inits.grouped(dim).toSeq.map(subSeq => factory.applyExpression(polymorphismFactory.typeReferenceExpression(arrayTpe), subSeq))
                  (outerArrayTpe, outerArrayExpr)
                }

              initializers._2.head

          case None =>
            val arrayTpe = dimensions.reverse.tail.foldLeft[any.Type]( 
              polymorphismFactory.typeApplication(createArray(dimensions.last), Seq(tpe))
             ) { case (arrayTpe, dimension) =>
              polymorphismFactory.typeApplication(createArray(dimension), Seq(arrayTpe))
            }
            polymorphismFactory.typeReferenceExpression(arrayTpe)
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
  val arraysOpsFactory: arraysOps.Factory
}
