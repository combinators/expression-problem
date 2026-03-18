package org.combinators.cogen.paradigm.ffi     /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class CreateArray[Type](elementType: Type, dimensions: Seq[Int])
case class Get()
case class Set()
case class Length()

trait Arrays[Context] extends FFI {
  import base._
  import syntax._

  trait ArrayCapabilities {
    implicit val canCreate: Understands[Context, Apply[CreateArray[Type], Expression, Expression]]
    def create(elemTpe: Type, dimensions: Seq[Int], contents: Seq[Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[CreateArray[Type], Expression, Expression](CreateArray(elemTpe, dimensions), contents))

    implicit val canGet: Understands[Context, Apply[Get, Expression, Expression]]
    def get(array: Expression, pos:Seq[Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Get, Expression, Expression](Get(), array +: pos))

    implicit val canSet: Understands[Context, Apply[Set, Expression, Expression]]
    def set(array: Expression, pos:Seq[Expression], value:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Set, Expression, Expression](Set(), array +: pos :+ value))

    implicit val canLength: Understands[Context, Apply[Length, Expression, Expression]]
    def length(array:Expression, dimension:Seq[Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Length, Expression, Expression](Length(), array +: dimension))
  }
  val arrayCapabilities: ArrayCapabilities
}

object Arrays {
  type WithBase[Ctxt, B <: AnyParadigm] = Arrays[Ctxt] { val base: B }
}
