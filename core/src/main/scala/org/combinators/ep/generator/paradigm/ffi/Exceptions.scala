package org.combinators.ep.generator.paradigm.ffi    /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}

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
