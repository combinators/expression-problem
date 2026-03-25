package org.combinators.cogen.paradigm.ffi     /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply, Reify}
import org.combinators.cogen.{Command, TypeRep, Understands}
import Command.Generator

// if have contents, must pass in Seq[Int] to be able to group one-dimensional sequence into appropriate sub-structures.
case class CreateArray[Type,Expression](elementType: Type, dimensions: Seq[Expression], contentSpec:Option[(Seq[Int], Seq[Expression])]) extends Command {
  override type Result = Expression
}

case class Get()
case class Set()
case class Length()

trait Arrays[Context] extends FFI {
  import base._
  import syntax._

  trait ArrayCapabilities {
    implicit val canCreate: Understands[Context, CreateArray[Type,Expression]]
    def create(elemTpe: Type, dimensions: Seq[Expression], contentSpec: Option[(Seq[Int], Seq[Expression])]): Generator[Context, Expression] =
      AnyParadigm.capability(CreateArray[Type,Expression](elemTpe, dimensions, contentSpec))
    
    def create(elemTpe: Type, dimensions: Seq[Int], contentSpec: Seq[Expression])
              (implicit reify: Understands[Context, Reify[Int, Expression]])
    : Generator[Context, Expression] = {
      import AnyParadigm.syntax.forEach
      for {
        dimExps <- forEach(dimensions) { dim => AnyParadigm.capability(Reify[Int, Expression](TypeRep.Int, dim)) }
        result <- create(elemTpe, dimExps, Some((dimensions, contentSpec)))
        } yield result
    }

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
