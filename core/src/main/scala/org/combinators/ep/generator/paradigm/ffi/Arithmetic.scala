package org.combinators.ep.generator.paradigm.ffi

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}

case class Add[T]()
case class Sub[T]()
case class Mult[T]()
case class Div[T]()
case class Mod[T]()

trait Arithmetic[Context, T] {
  val base: AnyParadigm

  import base.syntax._

  trait ArithmeticCapabilities {
    implicit val canAdd: Understands[Context, Apply[Add[T], Expression, Expression]]
    def add(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Add[T], Expression, Expression](Add[T](), xs))

    implicit val canSub: Understands[Context, Apply[Sub[T], Expression, Expression]]
    def sub(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Sub[T], Expression, Expression](Sub[T](), xs))

    implicit val canMult: Understands[Context, Apply[Mult[T], Expression, Expression]]
    def mult(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Mult[T], Expression, Expression](Mult[T](), xs))

    implicit val canDiv: Understands[Context, Apply[Div[T], Expression, Expression]]
    def div(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Div[T], Expression, Expression](Div[T](), xs))

    implicit val canMod: Understands[Context, Apply[Mod[T], Expression, Expression]]
    def mod(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Mod[T], Expression, Expression](Mod[T](), xs))
  }
  val arithmeticCapabilities: ArithmeticCapabilities
}

object Arithmetic {
  type WithBase[Ctxt, B <: AnyParadigm] = Arithmetic[Ctxt, B] { val base: B }
}
