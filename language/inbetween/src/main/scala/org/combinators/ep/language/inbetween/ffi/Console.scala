package org.combinators.ep.language.inbetween.ffi      /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Print, Console as Cnsl}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Console[AST <: ConsoleAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ConsoleInMethods extends Cnsl[_base.ast.any.Method] {
    val base: _base.type = _base
    import base.ast.consoleOpsFactory
    import base.ast.any

    val consoleCapabilities: ConsoleCapabilities = new ConsoleCapabilities {
      implicit val canPrint: Understands[any.Method, Apply[Print.type, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Print.type, any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Print.type, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, consoleOpsFactory.consolePrintOp(command.arguments.head))
          }
        }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
  val consoleInMethods: ConsoleInMethods = new ConsoleInMethods {}
}

object Console {
  type WithBase[AST <: ConsoleAST, B <: AnyParadigm.WithAST[AST]] = Console[AST, B] {}

  def apply[AST <: ConsoleAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Console[AST, B](_base) {}
}