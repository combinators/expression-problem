package org.combinators.ep.language.scala.paradigm.ffi

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Cos, EulersNumber, Log, Pi, Pow, Sin, Sqrt, RealArithmetic => RArith}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.ep.language.scala.paradigm.AnyParadigm
import org.combinators.ep.language.scala.Syntax.default.Expression

import scala.meta._

class RealArithmetic[Ctxt, T, AP <: AnyParadigm](
  val base: AP,
  rep: TypeRep.OfHostType[T],
  targetType: Type,
  reification: T => Expression
) extends RArith[Ctxt, T] {

  private def scalaMathOp[Ctxt, Op](methodName: String): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        val call =
          Term.Apply(
            Term.Select(
              Term.Select(
                Term.Name("scala"),
                Term.Name("math")
              ),
              Term.Name(methodName)
            ),
            command.arguments.toList
          )
        (context, call)
      }
    }

  private def scalaMathConst[Ctxt, Const <: Command.WithResult[Expression]](constName: String): Understands[Ctxt, Const] =
    new Understands[Ctxt, Const] {
      def perform(
        context: Ctxt,
        command: Const
      ): (Ctxt, Expression) = {
        val const =
          Term.Select(
            Term.Select(
              Term.Name("scala"),
              Term.Name("math")
            ),
            Term.Name(constName)
          )
        (context, const)
      }
    }

  val realArithmeticCapabilities: RealArithmeticCapabilities =
    new RealArithmeticCapabilities {
      implicit val canSqrt: Understands[Ctxt, Apply[Sqrt[T], Expression, Expression]] =
        scalaMathOp("sqrt")
      implicit val canPow: Understands[Ctxt, Apply[Pow[T], Expression, Expression]] =
        scalaMathOp("pow")
      implicit val canLog: Understands[Ctxt, Apply[Log[T], Expression, Expression]] =
        scalaMathOp("log")
      implicit val canSin: Understands[Ctxt, Apply[Sin[T], Expression, Expression]] =
        scalaMathOp("sin")
      implicit val canCos: Understands[Ctxt, Apply[Cos[T], Expression, Expression]] =
        scalaMathOp("cos")
      implicit val canEuler: Understands[Ctxt, EulersNumber[Expression]] =
        scalaMathConst("E")
      implicit val canPi: Understands[Ctxt, Pi[Expression]] =
        scalaMathConst("Pi")
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