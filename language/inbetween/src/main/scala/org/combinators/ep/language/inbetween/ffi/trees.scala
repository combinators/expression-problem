package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.{any, polymorphism}

import java.beans.Expression

object trees {

  trait FinalTypes extends operatorExpression.FinalTypes with polymorphism.FinalTypes {
    type CreateLeafExpr <: Expression
    type CreateNodeExpr <: Expression
  }
  trait CreateLeafExpr[FT <: FinalTypes] extends any.Expression[FT] {
    def getSelfCreateLeafExpr: finalTypes.CreateLeafExpr
  }

  trait CreateNodeExpr[FT <: FinalTypes] extends any.Expression[FT] {
    def getSelfCreateNodeExpr: finalTypes.CreateNodeExpr
  }
  trait Factory[FT <: FinalTypes] extends operatorExpression.Factory[FT] with polymorphism.Factory[FT] {
    def createNodeExpr(): CreateNodeExpr[FT]
    def createLeafExpr(): CreateLeafExpr[FT]

    def createNode(label: any.Expression[FT], children: Seq[any.Expression[FT]]): any.ApplyExpression[FT] =
      applyExpression(createNodeExpr(), label +: children)

    def createLeaf(tpe: any.Type[FT], value: any.Expression[FT]): any.ApplyExpression[FT] =
      applyExpression(createLeafExpr(), Seq(typeReferenceExpression(tpe), value))

    implicit def convert(other: CreateLeafExpr[FT]): CreateLeafExpr[FT]
    implicit def convert(other: CreateNodeExpr[FT]): CreateNodeExpr[FT]
  }
}