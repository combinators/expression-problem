package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.{any, polymorphism}

object ListOps {

  trait FinalTypes extends OperatorExpressionOps.FinalTypes with polymorphism.FinalTypes {

  }
  trait CreateList[FT <: FinalTypes] extends any.Type[FT]
  trait ConsListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait HeadListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait TailListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait AppendListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait Factory[FT <: FinalTypes] extends OperatorExpressionOps.Factory[FT] with polymorphism.Factory[FT] {
    def createList(): CreateList[FT]

    def createList(tpe: any.Type[FT], elems: Seq[any.Expression[FT]]): any.ApplyExpression[FT] =
      applyExpression(typeReferenceExpression(typeApplication(createList(), Seq(tpe))), elems)

    def consListOp(): ConsListOp[FT]
    def headListOp(): HeadListOp[FT]

    def tailListOp(): TailListOp[FT]

    def appendListOp(): AppendListOp[FT]

    def consList(element: any.Expression[FT], list: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(consListOp(), element, list)
    def head(list: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(headListOp(), list)

    def tail(list: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(tailListOp(), list)
    def appendList(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(appendListOp(), left, right)
  }
}
