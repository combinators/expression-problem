package org.combinators.cogen.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class Equals[Type](inType: Type)

trait Equality[Context] extends FFI  {
  import base.syntax._

  trait EqualityCapabilities {
    implicit val canEquals: Understands[Context, Apply[Equals[Type], Expression, Expression]]
    def areEqual(inType: Type, left: Expression, right: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Equals[Type], Expression, Expression](Equals(inType), Seq(left, right)))
  }
  val equalityCapabilities: EqualityCapabilities
}

object Equality {
  type WithBase[Ctxt, B <: AnyParadigm] = Equality[Ctxt] { val base: B }
}
