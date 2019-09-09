package org.combinators.ep.generator.paradigm.ffi

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}

case class GetTime()

trait Time[Context] {
  val base: AnyParadigm

  import base.syntax._

  trait TimeCapabilities {
    implicit val canGetTime: Understands[Context, Apply[GetTime, Expression, Expression]]
    def getTime(): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[GetTime, Expression, Expression](GetTime(), Seq.empty))
  }
  val timeCapabilities: TimeCapabilities
}

object Time {
  type WithBase[Ctxt, B <: AnyParadigm] = Time[Ctxt] { val base: B }
}
