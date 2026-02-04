package org.combinators.cogen.paradigm.ffi

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case object Print

trait Console[Context] extends FFI {
  import base.syntax._

  trait ConsoleCapabilities {
   
    implicit val canPrint: Understands[Context, Apply[Print.type, Expression, Expression]]
    def print(x: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Print.type, Expression, Expression](Print, Seq(x)))
  }
  
  val consoleCapabilities: ConsoleCapabilities
}

object Console {
  type WithBase[Ctxt, B <: AnyParadigm] = Console[Ctxt] { val base: B }
}
