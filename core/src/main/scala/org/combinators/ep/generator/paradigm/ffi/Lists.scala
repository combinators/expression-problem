package org.combinators.ep.generator.paradigm.ffi   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply, ParametricPolymorphism}

case class Create[Type](elementType: Type)
case class Cons()
case class Head()
case class Tail()
case class Append()

trait Lists[Context] extends FFI {
  import base._
  import syntax._

  trait ListCapabilities {
    implicit val canCreate: Understands[Context, Apply[Create[Type], Expression, Expression]]
    def create(elemTpe: Type, contents: Seq[Expression]): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Create[Type], Expression, Expression](Create(elemTpe), contents))

    implicit val canCons: Understands[Context, Apply[Cons, Expression, Expression]]
    def cons(elem: Expression, list: Expression): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Cons, Expression, Expression](Cons(), Seq(elem, list)))

    implicit val canHead: Understands[Context, Apply[Head, Expression, Expression]]
    def head(list: Expression): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Head, Expression, Expression](Head(), Seq(list)))

    implicit val canTail: Understands[Context, Apply[Tail, Expression, Expression]]
    def tail(list: Expression): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Tail, Expression, Expression](Tail(), Seq(list)))

    implicit val canAppend: Understands[Context, Apply[Append, Expression, Expression]]
    def append(prefix: Expression, suffix: Expression): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Append, Expression, Expression](Append(), Seq(prefix, suffix)))
  }
  val listCapabilities: ListCapabilities
}

object Lists {
  type WithBase[Ctxt, B <: AnyParadigm] = Lists[Ctxt] { val base: B }
}
