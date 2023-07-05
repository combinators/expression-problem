package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.{any, polymorphism}

object lists {

  trait FinalTypes extends operatorExpression.FinalTypes with polymorphism.FinalTypes {

  }
  trait CreateList[FT <: FinalTypes] extends any.Type[FT]
  trait ConsListOp[FT <: FinalTypes] extends operatorExpression.Operator[FT]
  trait HeadListOp[FT <: FinalTypes] extends operatorExpression.Operator[FT]
  trait TailListOp[FT <: FinalTypes] extends operatorExpression.Operator[FT]
  trait AppendListOp[FT <: FinalTypes] extends operatorExpression.Operator[FT]

  trait Factory[FT <: FinalTypes] extends operatorExpression.Factory[FT] with polymorphism.Factory[FT] {
    def createList(): CreateList[FT]

    def createList(tpe: any.Type[FT], elems: Seq[any.Expression[FT]]): any.ApplyExpression[FT] =
      applyExpression(typeReferenceExpression(typeApplication(createList(), Seq(tpe))), elems)

    def consListOp(): ConsListOp[FT]
    def headListOp(): HeadListOp[FT]

    def tailListOp(): TailListOp[FT]

    def appendListOp(): AppendListOp[FT]

    def consList(element: any.Expression[FT], list: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(consListOp(), element, list)
    def head(list: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(headListOp(), list)

    def tail(list: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(tailListOp(), list)
    def appendList(left: any.Expression[FT], right: any.Expression[FT]): operatorExpression.BinaryExpression[FT] =
      binaryExpression(appendListOp(), left, right)
  }
}