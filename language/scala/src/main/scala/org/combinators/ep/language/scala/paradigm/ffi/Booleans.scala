package org.combinators.ep.language.scala.paradigm.ffi

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{And, False, Not, Or, True, Booleans => Bools}
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.{ContextSpecificResolver, OperatorExprs, ProjectCtxt}
import org.combinators.ep.language.scala.paradigm.AnyParadigm

import scala.meta._

class Booleans[Ctxt, AP <: AnyParadigm](val base: AP) extends Bools[Ctxt] {
  import base.syntax._

  val booleanCapabilities: BooleanCapabilities =
    new BooleanCapabilities {
      implicit val canAnd: Understands[Ctxt, Apply[And, base.syntax.Expression, base.syntax.Expression]] =
        OperatorExprs.shortCutInfixExprOp[Ctxt, And](Term.Name("&&"), Term.Name("&"))
      implicit val canOr: Understands[Ctxt, Apply[Or, base.syntax.Expression, base.syntax.Expression]] =
        OperatorExprs.shortCutInfixExprOp[Ctxt, Or](Term.Name("||"), Term.Name("|"))
      implicit val canNot: Understands[Ctxt, Apply[Not, base.syntax.Expression, base.syntax.Expression]] =
        OperatorExprs.prefixExprOp[Ctxt, Not](Term.Name("!"))
      implicit val canTrue: Understands[Ctxt, True[base.syntax.Expression]] =
        new Understands[Ctxt, True[base.syntax.Expression]] {
          def perform(
            context: Ctxt,
            command: True[Expression]
          ): (Ctxt, Expression) = {
            (context, Lit.Boolean(true))
          }
        }
      implicit val canFalse: Understands[Ctxt, False[base.syntax.Expression]] =
        new Understands[Ctxt, False[base.syntax.Expression]] {
          def perform(
            context: Ctxt,
            command: False[Expression]
          ): (Ctxt, Expression) = {
            (context, Lit.Boolean(false))
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
          ContextSpecificResolver.updateResolver(base.config, TypeRep.Boolean, Type.Name("Boolean"))(Lit.Boolean(_))
        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      }
    })
}
