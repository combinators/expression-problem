package org.combinators.ep.generator.paradigm   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}

case class AddLowerBound[Type](bound: Type) extends Command {
  type Result = Unit
}

case class AddUpperBound[Type](bound: Type) extends Command {
  type Result = Unit
}

case class GetCurrentTypeParameter[Type]() extends Command {
  type Result = Type
}

trait Generics {
  val base: org.combinators.ep.generator.paradigm.AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[base.type]
  val ppolyParadigm: ParametricPolymorphism.WithBase[base.type]

  import base._
  import ooParadigm._
  import ppolyParadigm._
  import syntax._

  trait ClassCapabilities {
    implicit val canAddTypeParameterInClass: Understands[ClassContext, AddTypeParameter[Name, TypeParameterContext]]
    def addTypeParameter(name: Name, spec: Generator[TypeParameterContext, Unit]): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddTypeParameter[Name, TypeParameterContext](name, spec))

    implicit val canGetTypeArgumentsInClass: Understands[ClassContext, GetTypeArguments[Type]]
    def getTypeArguments(): Generator[ClassContext, Seq[Type]] =
      AnyParadigm.capability(GetTypeArguments[Type]())

    implicit val canApplyTypeInClass: Understands[ClassContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[ClassContext, Type] =
      AnyParadigm.capability(Apply[Type, Type, Type](tpe, arguments))
  }
  val classCapabilities: ClassCapabilities

  trait TypeParameterCapabilities {
    implicit val canGetCurrentTypeParameter: Understands[TypeParameterContext, GetCurrentTypeParameter[Type]]
    def getCurrentTypeParameter(): Generator[TypeParameterContext, Type] =
      AnyParadigm.capability(GetCurrentTypeParameter[Type]())

    implicit val canAddUpperBoundInTypeParameter: Understands[TypeParameterContext, AddUpperBound[Type]]
    def addUpperBound(tpe: Type): Generator[TypeParameterContext, Unit] =
      AnyParadigm.capability(AddUpperBound[Type](tpe))

    implicit val canAddLowerBoundInTypeParameter: Understands[TypeParameterContext, AddLowerBound[Type]]
    def addLowerBound(tpe: Type): Generator[TypeParameterContext, Unit] =
      AnyParadigm.capability(AddLowerBound[Type](tpe))

    implicit val canApplyTypeTypeParameter: Understands[TypeParameterContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[TypeParameterContext, Type] =
      AnyParadigm.capability(Apply[Type, Type, Type](tpe, arguments))
  }
  val typeParameterCapabilities: TypeParameterCapabilities

  trait ConstructorCapabilities {
    implicit val canApplyTypeInConstructor: Understands[ConstructorContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[ConstructorContext, Type] =
      AnyParadigm.capability(Apply[Type, Type, Type](tpe, arguments))

    implicit val canApplyMethodToTypeInConstructor: Understands[ConstructorContext, Apply[Expression, Type, Expression]]
    def instantiateTypeParameter(method: Expression, arguments: Seq[Type]): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(Apply[Expression, Type, Expression](method, arguments))
  }
  val constructorCapabilities: ConstructorCapabilities
}

object Generics {
  type WithBase[B <: AnyParadigm, OO <: ObjectOriented.WithBase[B], PP <: ParametricPolymorphism.WithBase[B]] =
    Generics { val base: B; val ooParadigm: OO; val ppolyParadigm: PP }
}