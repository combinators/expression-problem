package org.combinators.cogen.paradigm.ffi

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class Assert(message: String = "")

trait Assertions[Context] extends FFI {
  import base.syntax._

  trait AssertionCapabilities {
    implicit val canAssert: Understands[Context, Apply[Assert, Expression, Expression]]
    def assert(exp: Expression, message: String = ""): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Assert, Expression, Expression](Assert(message), Seq(exp)))

    def assertEquals
      (inTpe: Type, got: Expression, expected: Expression, message: String = "")
      (implicit canEqual: Understands[Context, Apply[Equals[Type], Expression, Expression]]): Generator[Context, Expression] = {
      for {
        eqExp <- Apply[Equals[Type], Expression, Expression](Equals[Type](inTpe), Seq(got, expected)).interpret
        res <- assert(eqExp, message)
      } yield res
    }

    def assertNotEquals
      (inTpe: Type, got: Expression, expected: Expression, message: String = "")
      (implicit
        canEqual: Understands[Context, Apply[Equals[Type], Expression, Expression]],
        canNegate: Understands[Context, Apply[Not, Expression, Expression]]
      ): Generator[Context, Expression] = {
      for {
        eqExp <- Apply[Equals[Type], Expression, Expression](Equals[Type](inTpe), Seq(got, expected)).interpret
        neqExp <- Apply[Not, Expression, Expression](Not(), Seq(eqExp)).interpret
        res <- assert(neqExp, message)
      } yield res
    }
  }
  val assertionCapabilities: AssertionCapabilities
}

object Assertions {
  type WithBase[Ctxt, B <: AnyParadigm] = Assertions[Ctxt] { val base: B }
}