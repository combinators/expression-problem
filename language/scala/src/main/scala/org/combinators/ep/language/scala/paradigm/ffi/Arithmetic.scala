package org.combinators.ep.language.scala.paradigm.ffi   /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Add, Div, LE, LT, Mod, Mult, Sub, Arithmetic => Arith}
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.ep.language.scala.paradigm.AnyParadigm

import scala.meta._
import org.combinators.ep.language.scala.OperatorExprs._

class Arithmetic[Ctxt, T, AP <: AnyParadigm](
  val base: AP,
  rep: TypeRep.OfHostType[T],
  targetType: Type,
  reification: T => Term
) extends Arith[Ctxt, T] {
  import base.syntax._

  val arithmeticCapabilities: ArithmeticCapabilities =
    new ArithmeticCapabilities {
      implicit val canAdd: Understands[Ctxt, Apply[Add[T], Expression, Expression]] =
        infixExprOp(Term.Name("+"))
      implicit val canSub: Understands[Ctxt, Apply[Sub[T], Expression, Expression]] =
        infixExprOp(Term.Name("-"))
      implicit val canMult: Understands[Ctxt, Apply[Mult[T], Expression, Expression]] =
        infixExprOp(Term.Name("*"))
      implicit val canDiv: Understands[Ctxt, Apply[Div[T], Expression, Expression]] =
        infixExprOp(Term.Name("/"))
      implicit val canMod: Understands[Ctxt, Apply[Mod[T], Expression, Expression]] =
        infixExprOp(Term.Name("%"))
      implicit val canLT: Understands[Ctxt, Apply[LT[T], Term, Term]] =
        infixExprOp(Term.Name("<"))
      implicit val canLE: Understands[Ctxt, Apply[LE[T], Term, Term]] =
        infixExprOp(Term.Name("<="))
    }
  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        val resolverUpdate =
          ContextSpecificResolver.updateResolver(base.config, rep, targetType)(reification)(_)
        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      }
    })
}
