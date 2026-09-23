package org.combinators.ep.language.inbetween.ffi      /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Print, Console as Cnsl}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.any.AnyParadigm.WithAST

trait Console {
  val _base: AnyParadigm { val ast: ConsoleAST }
  trait ConsoleIn[Ctxt] extends Cnsl[Ctxt] with FFI[Ctxt] {
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
  type WithBase[B <: AnyParadigm] = Console { val _base: B }
  def apply[AST <: ConsoleAST, B <: AnyParadigm.WithAST[AST]](base: B): WithBase[base.type] = {
    class Cnsl(override val _base: base.type = base) extends Console {}
    new Cnsl()
  }
}