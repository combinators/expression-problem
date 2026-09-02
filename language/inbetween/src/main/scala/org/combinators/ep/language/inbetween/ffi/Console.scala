package org.combinators.ep.language.inbetween.ffi      /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Print, Console as Cnsl}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Console[AST <: ConsoleAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ConsoleIn[Ctxt] extends Cnsl[Ctxt] {
    val base: _base.type = _base
    import base.ast.consoleOpsFactory
    import base.ast.any

    val consoleCapabilities: ConsoleCapabilities = new ConsoleCapabilities {
      implicit val canPrint: Understands[Ctxt, Apply[Print.type, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Print.type, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Print.type, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, consoleOpsFactory.consolePrintOp(command.arguments.head))
          }
        }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
}

object Console {
  type WithBase[AST <: ConsoleAST, B <: AnyParadigm.WithAST[AST]] = Console[AST, B] {}

  def apply[AST <: ConsoleAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Console[AST, B](_base) {}
}