package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.ffi.{Exception, Exceptions as Excptns}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Exceptions[AST <: ExceptionsAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ExceptionsIn[Ctxt] extends Excptns[Ctxt] {
    val base: _base.type = _base
    import base.ast.exceptionsOpsFactory
    import base.ast.any
    import base.ast

    val exceptionsCapabilities: ExceptionsCapabilities = new ExceptionsCapabilities {

      implicit val canRaise: Understands[Ctxt, Exception[any.Expression, any.Statement]] =
        new Understands[Ctxt,Exception[any.Expression, any.Statement]] {
          def perform(context: Ctxt, command: Exception[any.Expression, any.Statement]): (Ctxt, any.Statement) = {
            val expr = exceptionsOpsFactory.raiseOp(command.exp)

            // Need to convert this EXPR into a STATEMENT
            (context, ???)
          }
        }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
}

object Exceptions {
  type WithBase[AST <: ExceptionsAST, B <: AnyParadigm.WithAST[AST]] = Exceptions[AST, B] {}

  def apply[AST <: ExceptionsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Exceptions[AST, B](_base) {}
}