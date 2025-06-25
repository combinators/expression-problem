package org.combinators.cogen.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class GetTime()

trait Time[Context] extends FFI {
  import base.syntax._

  trait TimeCapabilities {
    implicit val canGetTime: Understands[Context, Apply[GetTime, Expression, Expression]]
    def getTime(): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[GetTime, Expression, Expression](GetTime(), Seq.empty))
  }
  val timeCapabilities: TimeCapabilities
}

object Time {
  type WithBase[Ctxt, B <: AnyParadigm] = Time[Ctxt] { val base: B }
}
