package org.combinators.ep.generator.paradigm.ffi   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}

case object Print

trait Console[Context] extends FFI {
  import base.syntax._

  trait ConsoleCapabilities {
   
    implicit val canPrint: Understands[Context, Apply[Print.type, Expression, Expression]]
    def print(x: Expression): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Print.type, Expression, Expression](Print, Seq(x)))
  }
  
  val consoleCapabilities: ConsoleCapabilities
}

object Console {
  type WithBase[Ctxt, B <: AnyParadigm] = Console[Ctxt] { val base: B }
}
