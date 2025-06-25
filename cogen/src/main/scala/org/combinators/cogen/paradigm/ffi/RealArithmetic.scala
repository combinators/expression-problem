package org.combinators.cogen.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class Sqrt[T]()
case class Pow[T]()
case class Log[T]()
case class Sin[T]()
case class Cos[T]()

case class Abs[T]()
case class Floor[T]()

case class EulersNumber[T]() extends Command {
  type Result = T
}
case class Pi[T]() extends Command {
  type Result = T
}

trait RealArithmetic[Context, T] extends FFI {
  import base.syntax._

  trait RealArithmeticCapabilities {
    implicit val canSqrt: Understands[Context, Apply[Sqrt[T], Expression, Expression]]
    def sqrt(x: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Sqrt[T], Expression, Expression](Sqrt[T](), Seq(x)))

    implicit val canPow: Understands[Context, Apply[Pow[T], Expression, Expression]]
    def pow(base: Expression, exponent: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Pow[T], Expression, Expression](Pow[T](), Seq(base, exponent)))

    implicit val canLog: Understands[Context, Apply[Log[T], Expression, Expression]]
    def log(base: Expression, x: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Log[T], Expression, Expression](Log[T](), Seq(base, x)))

    implicit val canSin: Understands[Context, Apply[Sin[T], Expression, Expression]]
    def sin(x: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Sin[T], Expression, Expression](Sin[T](), Seq(x)))

    implicit val canCos: Understands[Context, Apply[Cos[T], Expression, Expression]]
    def cos(x: Expression): Generator[Context, Expression]  =
      AnyParadigm.capability(Apply[Cos[T], Expression, Expression](Cos[T](), Seq(x)))

    implicit val canEuler: Understands[Context, EulersNumber[Expression]]
    def eulersNumber(): Generator[Context, Expression] =
      AnyParadigm.capability(EulersNumber[Expression]())

    implicit val canPi: Understands[Context, Pi[Expression]]
    def pi(): Generator[Context, Expression] =
      AnyParadigm.capability(Pi[Expression]())

    implicit val canAbs: Understands[Context, Apply[Abs[T], Expression, Expression]]
    def abs(xs: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Abs[T], Expression, Expression](Abs[T](), Seq(xs)))

    implicit val canFloor: Understands[Context, Apply[Floor[T], Expression, Expression]]
    def floor(xs: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Floor[T], Expression, Expression](Floor[T](), Seq(xs)))
  }
  val realArithmeticCapabilities: RealArithmeticCapabilities
}

object RealArithmetic {
  type WithBase[Ctxt, B <: AnyParadigm, T] = RealArithmetic[Ctxt, T] { val base: B }
}
