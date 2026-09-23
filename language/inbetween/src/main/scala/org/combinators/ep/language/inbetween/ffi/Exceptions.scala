package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.ffi.{Exception, Exceptions as Excptns}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.any.AnyParadigm.WithAST

trait Exceptions[AST <: ExceptionsAST, B] {
  val _base: AnyParadigm.WithAST[AST] & B
  trait ExceptionsIn[Ctxt] extends Excptns[Ctxt] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.exceptionsOpsFactory
    override val base: _base.type = _base

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
  }
}

object Exceptions {
  type WithBase[AST <: ExceptionsAST, B <: AnyParadigm.WithAST[AST]] = Exceptions[AST, B] {}

  def apply[AST <: ExceptionsAST, B <: AnyParadigm.WithAST[AST]](base: B): WithBase[AST, base.type] = new Exceptions[AST, base.type] {
    override val _base: base.type = base
  }
}