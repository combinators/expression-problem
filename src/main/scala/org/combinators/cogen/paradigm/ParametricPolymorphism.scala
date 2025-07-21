package org.combinators.cogen.paradigm

import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class AddTypeParameter[Name, TypeParameterContext](
    name: Name,
    spec: Generator[TypeParameterContext, Unit]
  ) extends Command {
  type Result = Unit
}

case class GetTypeArguments[Type]() extends Command {
  type Result = Seq[Type]
}

trait ParametricPolymorphism {
  val base: AnyParadigm

  import base._
  import syntax._

  type TypeParameterContext

  trait MethodBodyCapabilities {
    implicit val canAddTypeParameterInMethod: Understands[MethodBodyContext, AddTypeParameter[Name, TypeParameterContext]]
    def addTypeParameter(name: Name, spec: Generator[TypeParameterContext, Unit]): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(AddTypeParameter[Name, TypeParameterContext](name, spec))

    implicit val canGetTypeArgumentsInMethod: Understands[MethodBodyContext, GetTypeArguments[Type]]
    def getTypeArguments(): Generator[MethodBodyContext, Seq[Type]] =
      AnyParadigm.capability(GetTypeArguments[Type]())

    implicit val canApplyTypeInMethod: Understands[MethodBodyContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[MethodBodyContext, Type] =
      AnyParadigm.capability(Apply[Type, Type, Type](tpe, arguments))

    implicit val canApplyMethodToTypeInMethod: Understands[MethodBodyContext, Apply[Expression, Type, Expression]]
    def instantiateTypeParameter(method: Expression, arguments: Seq[Type]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(Apply[Expression, Type, Expression](method, arguments))
  }
  val methodBodyCapabilities: MethodBodyCapabilities

}

object ParametricPolymorphism {
  type WithBase[B <: AnyParadigm] = ParametricPolymorphism { val base: B }
}