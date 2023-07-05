package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.{any, polymorphism}

import java.beans.Expression

object trees {

  trait FinalTypes extends operatorExpression.FinalTypes with polymorphism.FinalTypes {
    type CreateLeaf <: Type
    type CreateNodeExpr <: Expression
  }
  trait CreateLeaf[FT <: FinalTypes] extends any.Type[FT] {
    def getSelfCreateLeaf: finalTypes.CreateLeaf
  }

  trait CreateNodeExpr[FT <: FinalTypes] extends any.Expression[FT] {
    def getSelfCreateNodeExpr: finalTypes.CreateNodeExpr
  }
  trait Factory[FT <: FinalTypes] extends operatorExpression.Factory[FT] with polymorphism.Factory[FT] {
    def createNodeExpr(): CreateNodeExpr[FT]
    def createLeaf(): CreateLeaf[FT]

    def createNode(label: any.Expression[FT], children: Seq[any.Expression[FT]]): any.ApplyExpression[FT] =
      applyExpression(createNodeExpr(), label +: children)

    def createLeaf(tpe: any.Type[FT], value: any.Expression[FT]): any.ApplyExpression[FT] =
      applyExpression(typeReferenceExpression(typeApplication(createLeaf(), Seq(tpe))), Seq(value))

    implicit def convert(other: CreateLeaf[FT]): CreateLeaf[FT]
    implicit def convert(other: CreateNodeExpr[FT]): CreateNodeExpr[FT]
  }
}