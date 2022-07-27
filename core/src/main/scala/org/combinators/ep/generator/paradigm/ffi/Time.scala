package org.combinators.ep.generator.paradigm.ffi   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}

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
