package org.combinators.ep.generator.paradigm.ffi

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}

case class Equals[Type](inType: Type)

trait Equality[Context] extends FFI  {
  import base.syntax._

  trait EqualityCapabilities {
    implicit val canEquals: Understands[Context, Apply[Equals[Type], Expression, Expression]]
    def areEqual(inType: Type, left: Expression, right: Expression): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Equals[Type], Expression, Expression](Equals(inType), Seq(left, right)))
  }
  val equalityCapabilities: EqualityCapabilities
}

object Equality {
  type WithBase[Ctxt, B <: AnyParadigm] = Equality[Ctxt] { val base: B }
}
