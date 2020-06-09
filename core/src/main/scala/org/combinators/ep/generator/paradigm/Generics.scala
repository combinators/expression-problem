package org.combinators.ep.generator.paradigm   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}

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

  trait ClassCapabilities {
    implicit val canAddTypeParameterInClass: Understands[ClassContext, AddTypeParameter[Name, TypeParameterContext]]
    def addTypeParameter(name: Name, spec: Generator[TypeParameterContext, Unit]): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddTypeParameter[Name, TypeParameterContext](name, spec))

    implicit val canGetTypeArgumentsInClass: Understands[ClassContext, GetTypeArguments[Type]]
    def getTypeArguments(): Generator[ClassContext, Seq[Type]] =
      AnyParadigm.capabilitiy(GetTypeArguments[Type]())

    implicit val canApplyTypeInClass: Understands[ClassContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[ClassContext, Type] =
      AnyParadigm.capabilitiy(Apply[Type, Type, Type](tpe, arguments))
  }
  val classCapabilities: ClassCapabilities

  trait TypeParameterCapabilities {
    implicit val canAddUpperBoundInTypeParameter: Understands[TypeParameterContext, AddUpperBound[Type]]
    implicit val canAddLowerBoundInTypeParameter: Understands[TypeParameterContext, AddLowerBound[Type]]

    implicit val canApplyTypeTypeParameter: Understands[TypeParameterContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[TypeParameterContext, Type] =
      AnyParadigm.capabilitiy(Apply[Type, Type, Type](tpe, arguments))
  }
  val typeParameterCapabilities: TypeParameterCapabilities

  trait ConstructorCapabilities {
    implicit val canApplyTypeInConstructor: Understands[ConstructorContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[ConstructorContext, Type] =
      AnyParadigm.capabilitiy(Apply[Type, Type, Type](tpe, arguments))

    implicit val canApplyMethodToTypeInConstructor: Understands[ConstructorContext, Apply[Expression, Type, Expression]]
    def instantiateTypeParameter(method: Expression, arguments: Seq[Type]): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(Apply[Expression, Type, Expression](method, arguments))
  }
  val constructorCapabilities: ConstructorCapabilities
}

object Generics {
  type WithBase[B <: AnyParadigm, OO <: ObjectOriented.WithBase[B], PP <: ParametricPolymorphism.WithBase[B]] =
    Generics { val base: B; val ooParadigm: OO; val ppolyParadigm: PP }
}