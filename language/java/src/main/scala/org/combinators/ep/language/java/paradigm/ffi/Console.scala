package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.expr.{FieldAccessExpr, MethodCallExpr, NameExpr, SimpleName}
import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Print, Strings, Console as Con}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.Understands
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.paradigm.AnyParadigm

class Console[Ctxt, AP <: AnyParadigm](
  val base: AP,
  strings: Strings.WithBase[Ctxt, AP]
) extends Con[Ctxt] {

  val consoleCapabilities: ConsoleCapabilities =
    new ConsoleCapabilities {

      implicit val canPrint: Understands[Ctxt, Apply[Print.type, Expression, Expression]] =
        new Understands[Ctxt, Apply[Print.type, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[Print.type, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, new MethodCallExpr(new FieldAccessExpr(new NameExpr(new SimpleName("System")), "out"), "println",
              new NodeList(command.arguments.head))
              )
          }
        }
    }

  def enable(): Generator[base.ProjectContext, Unit] = strings.enable()
}
