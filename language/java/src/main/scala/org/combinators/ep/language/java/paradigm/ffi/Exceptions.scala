package org.combinators.ep.language.java.paradigm.ffi     /*DI:LD:AI*/

import com.github.javaparser.ast.{ImportDeclaration, NodeList}
import com.github.javaparser.ast.expr.{BooleanLiteralExpr, ObjectCreationExpr}
import com.github.javaparser.ast.stmt.ThrowStmt
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.{Apply, ffi}
import org.combinators.ep.generator.paradigm.ffi.{Assert, Exceptions => Excptns}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.language.java.{ContextSpecificResolver, MethodBodyCtxt, ProjectCtxt}

class Exceptions[AP <: AnyParadigm](val base: AP) extends Excptns[MethodBodyCtxt] {

  val exceptionCapabilities: ExceptionCapabilities =
    new ExceptionCapabilities {
      override implicit val canRaise: Understands[MethodBodyCtxt, ffi.Exception[Expression, Statement]] = {
        new Understands[MethodBodyCtxt, ffi.Exception[Expression, Statement]] {
          def perform(
                       context: MethodBodyCtxt,
                       command: ffi.Exception[Expression, Statement]
                     ): (MethodBodyCtxt, Statement) = {
            val ex = new ObjectCreationExpr(null, ObjectOriented.nameToType(ObjectOriented.fromComponents("RuntimeException")), new NodeList(command.exp))
            val throwStatement = new ThrowStmt(ex)
            (context, throwStatement)
          }
        }
      }
    }

  override def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        (context, ())
      }
    })
}
