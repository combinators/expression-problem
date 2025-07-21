package org.combinators.cogen.paradigm.ffi

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class Add[T]()
case class Sub[T]()
case class Mult[T]()
case class Div[T]()
case class Mod[T]()

case class LE[T]()
case class LT[T]()

// Foreign Function Interface (foreign to Scala for example)
trait Arithmetic[Context, T] extends FFI {
  import base.syntax._

  trait ArithmeticCapabilities {
    implicit val canLT:Understands[Context, Apply[LT[T], Expression, Expression]]
    def lt(left: Expression, right:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[LT[T], Expression, Expression](LT[T](), Seq(left, right)))

    implicit val canLE:Understands[Context, Apply[LE[T], Expression, Expression]]
    def le(left: Expression, right:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[LE[T], Expression, Expression](LE[T](), Seq(left, right)))

    implicit val canAdd: Understands[Context, Apply[Add[T], Expression, Expression]]
    def add(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Add[T], Expression, Expression](Add[T](), xs))

    implicit val canSub: Understands[Context, Apply[Sub[T], Expression, Expression]]
    def sub(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Sub[T], Expression, Expression](Sub[T](), xs))

    implicit val canMult: Understands[Context, Apply[Mult[T], Expression, Expression]]
    def mult(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Mult[T], Expression, Expression](Mult[T](), xs))

    implicit val canDiv: Understands[Context, Apply[Div[T], Expression, Expression]]
    def div(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Div[T], Expression, Expression](Div[T](), xs))

    implicit val canMod: Understands[Context, Apply[Mod[T], Expression, Expression]]
    def mod(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Mod[T], Expression, Expression](Mod[T](), xs))
  }
  val arithmeticCapabilities: ArithmeticCapabilities
}

object Arithmetic {
  type WithBase[Ctxt, B <: AnyParadigm, T] = Arithmetic[Ctxt, T] { val base: B }
}
