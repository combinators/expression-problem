package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait ListsAST extends OperatorExpressionOpsAST with ParametricPolymorphismAST {
  object listsOps {
    trait CreateList extends any.Type

    trait ConsListOp extends operatorExpressions.Operator

    trait HeadListOp extends operatorExpressions.Operator

    trait TailListOp extends operatorExpressions.Operator

    trait AppendListOp extends operatorExpressions.Operator

    trait Factory {
      def createList(): CreateList

      def createList(tpe: any.Type, elems: Seq[any.Expression]): any.ApplyExpression =
        factory.applyExpression(polymorphismFactory.typeReferenceExpression(polymorphismFactory.typeApplication(createList(), Seq(tpe))), elems)

      def consListOp(): ConsListOp
      def headListOp(): HeadListOp

      def tailListOp(): TailListOp

      def appendListOp(): AppendListOp

      def consList(element: any.Expression, list: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(consListOp(), element, list)
      def head(list: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(headListOp(), list)

      def tail(list: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(tailListOp(), list)
      def appendList(left: any.Expression, right: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(appendListOp(), left, right)
    }
  }
  val listsOpsFactory: listsOps.Factory
}