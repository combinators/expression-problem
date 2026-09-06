package org.combinators.cogen.paradigm.ffi     /*DI:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class Exception[Expression,Stmt](exp:Expression) extends Command {
  type Result = Stmt
}

trait Exceptions[Context, T] extends FFI {
  import base.syntax._

  trait ExceptionsCapabilities {
    implicit val canRaise: Understands[Context, Exception[Expression, Statement]]

    def raise(exp: Expression): Generator[Context, Statement] =
      AnyParadigm.capability(Exception[Expression, Statement](exp))
  }
  
  val exceptionsCapabilities: ExceptionsCapabilities
}

object Exceptions {
  type WithBase[Ctxt, B <: AnyParadigm, T] = Exceptions[Ctxt, T] { val base: B }
}
