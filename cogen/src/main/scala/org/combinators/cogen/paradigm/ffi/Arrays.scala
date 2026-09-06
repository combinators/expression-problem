package org.combinators.cogen.paradigm.ffi     /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply, Reify}
import org.combinators.cogen.{Command, TypeRep, Understands}
import Command.Generator

// if this has contents, must pass in Seq[Int] to be able to group one-dimensional sequence into appropriate sub-structures.
case class CreateArray[Type,Expression](elementType: Type, dimensions: Seq[Expression], contentSpec:Option[(Seq[Int], Seq[Expression])]) extends Command {
  override type Result = Expression
}

case class Get()
case class Set()
case class Length()

trait Arrays[Context, T] extends FFI {
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
    def get(array: Expression, indices:Seq[Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Get, Expression, Expression](Get(), array +: indices))

    // construct Sequence as (array, newValue, indices) since there is always a single newValue but may be multiple indices
    implicit val canSet: Understands[Context, Apply[Set, Expression, Expression]]
    def set(array: Expression, indices:Seq[Expression], value:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Set, Expression, Expression](Set(), Seq(array, value) ++ indices))

    implicit val canLength: Understands[Context, Apply[Length, Expression, Expression]]
    def length(array:Expression, dimensions:Seq[Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Length, Expression, Expression](Length(), array +: dimensions))
  }
  val arrayCapabilities: ArrayCapabilities
}

object Arrays {
  type WithBase[Ctxt, B <: AnyParadigm, T] = Arrays[Ctxt, T] { val base: B }
}
