package org.combinators.ep.language.java.paradigm.ffi

import com.github.javaparser.ast.expr.{BinaryExpr, Expression, MethodCallExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Abs, Cos, EulersNumber, Floor, Log, Pi, Pow, Sin, Sqrt, RealArithmetic => RArith}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.paradigm.AnyParadigm
import org.combinators.ep.language.java.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.templating.twirl.Java

class RealArithmetic[Ctxt, T, AP <: AnyParadigm](
  val base: AP,
  rep: TypeRep.OfHostType[T],
  targetType: Type,
  reification: T => Expression
) extends RArith[Ctxt, T] {
  import org.combinators.ep.language.java.OperatorExprs._

  private def javaMathOp[Ctxt, Op](methodName: String): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, Java(s"Math.${methodName}(${command.arguments.mkString(", ")})").expression())
      }
    }

  // Math.Log in java assumes base of E
  private def javaMathLogOp[Ctxt, Op](): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
                   context: Ctxt,
                   command: Apply[Op, Expression, Expression]
                 ): (Ctxt, Expression) = {
        (context, Java(s"Math.log(${command.arguments(0)})/Math.log(${command.arguments(1)})").expression())
      }
    }

  private def javaMathConst[Ctxt, Const <: Command.WithResult[Expression]](constName: String): Understands[Ctxt, Const] =
    new Understands[Ctxt, Const] {
      def perform(
        context: Ctxt,
        command: Const
      ): (Ctxt, Expression) = {
        (context, Java(s"Math.${constName}").expression())
      }
    }

  val realArithmeticCapabilities: RealArithmeticCapabilities =
    new RealArithmeticCapabilities {
      implicit val canSqrt: Understands[Ctxt, Apply[Sqrt[T], Expression, Expression]] =
        javaMathOp("sqrt")
      implicit val canPow: Understands[Ctxt, Apply[Pow[T], Expression, Expression]] =
        javaMathOp("pow")
      implicit val canLog: Understands[Ctxt, Apply[Log[T], Expression, Expression]] =
        javaMathLogOp()
      implicit val canSin: Understands[Ctxt, Apply[Sin[T], Expression, Expression]] =
        javaMathOp("sin")
      implicit val canCos: Understands[Ctxt, Apply[Cos[T], Expression, Expression]] =
        javaMathOp("cos")
      implicit val canEuler: Understands[Ctxt, EulersNumber[Expression]] =
        javaMathConst("E")
      implicit val canPi: Understands[Ctxt, Pi[Expression]] =
        javaMathConst("PI")
      implicit val canAbs: Understands[Ctxt, Apply[Abs[T], Expression, Expression]] =
        javaMathOp("abs")
      implicit val canFloor: Understands[Ctxt, Apply[Floor[T], Expression, Expression]] =
        javaMathOp("floor")
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