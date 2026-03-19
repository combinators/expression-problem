package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait ArraysAST extends OperatorExpressionOpsAST with ParametricPolymorphismAST {
  object arraysOps {
    trait CreateArray extends any.Type

    trait GetArrayOp extends operatorExpressions.Operator
    trait SetArrayOp extends operatorExpressions.Operator
    trait LengthArrayOp extends operatorExpressions.Operator

    trait Factory {
      def createArray(): CreateArray

      def createArray(tpe: any.Type, elems: Seq[any.Expression]): any.ApplyExpression =
        factory.applyExpression(polymorphismFactory.typeReferenceExpression(polymorphismFactory.typeApplication(createArray(), Seq(tpe))), elems)

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
