package org.combinators.cogen.paradigm.ffi

import org.combinators.cogen.paradigm.{AnyParadigm}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class Exception[Expression,Stmt](exp:Expression) extends Command {
  type Result = Stmt
}

trait Exceptions[Context] extends FFI {
  import base.syntax._

  trait ExceptionCapabilities {
    implicit val canRaise: Understands[Context, Exception[Expression, Statement]]

    def raise(exp: Expression): Generator[Context, Statement] =
      AnyParadigm.capability(Exception[Expression, Statement](exp))
  }
  
  val exceptionCapabilities: ExceptionCapabilities
}

object Exceptions {
  type WithBase[Ctxt, B <: AnyParadigm] = Exceptions[Ctxt] { val base: B }
}
