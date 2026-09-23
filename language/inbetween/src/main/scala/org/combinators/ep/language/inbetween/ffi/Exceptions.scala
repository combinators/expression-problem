package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.ffi.{Exception, Exceptions as Excptns}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.any.AnyParadigm.WithAST

trait Exceptions{
  val _base: AnyParadigm { val ast: ExceptionsAST }
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
  type WithBase[B <: AnyParadigm] = Exceptions { val _base: B }

  def apply[AST <: ExceptionsAST, B <: AnyParadigm.WithAST[AST]](base: B): WithBase[base.type] = {
    class Expts(override val _base: base.type = base) extends Exceptions {}
    new Expts()
  }
}