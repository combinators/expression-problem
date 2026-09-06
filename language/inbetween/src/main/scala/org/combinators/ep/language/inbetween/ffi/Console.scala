package org.combinators.ep.language.inbetween.ffi      /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Print, Console as Cnsl}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Console[AST <: ConsoleAST, B, T](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ConsoleIn[Ctxt] extends Cnsl[Ctxt, T] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.consoleOpsFactory
    override val base: _base.type = _base
 
    val consoleCapabilities: ConsoleCapabilities = new ConsoleCapabilities {
      implicit val canPrint: Understands[Ctxt, Apply[Print.type, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Print.type, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Print.type, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, consoleOpsFactory.consolePrintOp(command.arguments.head))
          }
        }
    }
  }
}

object Console {
  type WithBase[T, AST <: ConsoleAST, B <: AnyParadigm.WithAST[AST]] = Console[AST, B, T] {}
  def apply[T, AST <: ConsoleAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[T, AST, B] = new Console[AST, B, T](_base) {}
}