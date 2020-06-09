package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.expr.{BinaryExpr, BooleanLiteralExpr, UnaryExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Booleans => Bools, _}
import org.combinators.ep.language.java.paradigm.AnyParadigm
import org.combinators.ep.language.java.{ContextSpecificResolver, OperatorExprs, ProjectCtxt, Syntax}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.templating.twirl.Java
import Syntax.default._


class Booleans[Ctxt, AP <: AnyParadigm](val base: AP) extends Bools[Ctxt] {
  val booleanCapabilities: BooleanCapabilities =
    new BooleanCapabilities {
      implicit val canAnd: Understands[Ctxt, Apply[And, base.syntax.Expression, base.syntax.Expression]] =
        OperatorExprs.shortCutInfixExprOp[Ctxt, And](BinaryExpr.Operator.AND, BinaryExpr.Operator.BINARY_AND)
      implicit val canOr: Understands[Ctxt, Apply[Or, base.syntax.Expression, base.syntax.Expression]] =
        OperatorExprs.shortCutInfixExprOp[Ctxt, Or](BinaryExpr.Operator.OR, BinaryExpr.Operator.BINARY_OR)
      implicit val canNot: Understands[Ctxt, Apply[Not, base.syntax.Expression, base.syntax.Expression]] =
        OperatorExprs.prefixExprOp[Ctxt, Not](UnaryExpr.Operator.LOGICAL_COMPLEMENT)
      implicit val canTrue: Understands[Ctxt, True[base.syntax.Expression]] =
        new Understands[Ctxt, True[base.syntax.Expression]] {
          def perform(
            context: Ctxt,
            command: True[Expression]
          ): (Ctxt, Expression) = {
            (context, new BooleanLiteralExpr(true))
          }
        }
      implicit val canFalse: Understands[Ctxt, False[base.syntax.Expression]] =
        new Understands[Ctxt, False[base.syntax.Expression]] {
          def perform(
            context: Ctxt,
            command: False[Expression]
          ): (Ctxt, Expression) = {
            (context, new BooleanLiteralExpr(false))
          }
        }
    }
  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        val resolverUpdate =
          ContextSpecificResolver.updateResolver(base.config, TypeRep.Boolean, Java("boolean").tpe())(new BooleanLiteralExpr(_))
        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      }
    })
}
