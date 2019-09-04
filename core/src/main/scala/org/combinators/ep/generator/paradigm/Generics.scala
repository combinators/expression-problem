package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}

case class AddLowerBound[Type](bound: Type) extends Command {
  type Result = Unit
}

case class AddUpperBound[Type](bound: Type) extends Command {
  type Result = Unit
}

trait Generics {
  val base: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[base.type]
  val ppolyParadigm: ParametricPolymorphism.WithBase[base.type]

  import base._
  import ooParadigm._
  import ppolyParadigm._
  import syntax._

  implicit val canAddTypeParameterInClass: Understands[ClassContext, AddTypeParameter[TypeParameterContext, Type]]
  implicit val canApplyTypeInClass: Understands[ClassContext, Apply[Type]]
  implicit val canAddUpperBoundInTypeParameter: Understands[TypeParameterContext, AddUpperBound[Type]]
  implicit val canAddLowerBoundInTypeParameter: Understands[TypeParameterContext, AddLowerBound[Type]]
}

object Generics {
  type WithBase[B <: AnyParadigm, OO <: ObjectOriented.WithBase[B], PP <: ParametricPolymorphism.WithBase[B]] =
    Generics { val base: B; val ooParadigm: OO; val ppolyParadigm: PP }
}