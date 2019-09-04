package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import org.combinators.ep.generator.Command.Generator

case class AddTypeParameter[TypeParameterContext, Type](
    name: String,
    spec: Generator[TypeParameterContext, Unit]
  ) extends Command {
  type Result = Type
}

trait ParametricPolymorphism {
  val base: AnyParadigm

  import base._
  import syntax._
  import Command._

  type TypeParameterContext

  implicit val canAddTypeParameterInMethod: Understands[MethodBodyContext, AddTypeParameter[TypeParameterContext, Type]]
  implicit val canApplyTypeInMethod: Understands[MethodBodyContext, Apply[Type]]

  def emptyTypeParameterSpec: Generator[TypeParameterContext, Unit] = Command.monadInstance.pure(())
}

object ParametricPolymorphism {
  type WithBase[B <: AnyParadigm] = ParametricPolymorphism { val base: B }
}