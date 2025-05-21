package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.expr.BinaryExpr.Operator
import com.github.javaparser.ast.expr.{BinaryExpr, Expression, FieldAccessExpr, MethodCallExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Abs, Cos, EulersNumber, Floor, Log, Pi, Pow, Sin, Sqrt, RealArithmetic => RArith}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.language.java.{ContextSpecificResolver, ProjectCtxt}


class RealArithmetic[Ctxt, T, AP <: AnyParadigm](
  val base: AP,
  rep: TypeRep.OfHostType[T],
  targetType: Type,
  reification: T => Expression
) extends RArith[Ctxt, T] {
  import org.combinators.ep.language.java.OperatorExprs._

  case object RealArithmeticEnabled

  val math = ObjectOriented.fromComponents("Math")
  val mathExp = ObjectOriented.nameToExpression(math)

  private def javaMathOp[Ctxt, Op](methodName: String): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, new MethodCallExpr(mathExp, methodName, new NodeList[Expression](command.arguments*)))
      }
    }

  // Math.Log in java assumes base of E
  private def javaMathLogOp[Ctxt, Op](): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
                   context: Ctxt,
                   command: Apply[Op, Expression, Expression]
                 ): (Ctxt, Expression) = {
        def logOp(arg: Expression): Expression =
          new MethodCallExpr(mathExp, "log", new NodeList[Expression](arg))
        (context,
          new BinaryExpr(
            logOp(command.arguments(0)),
            logOp(command.arguments(1)),
            Operator.DIVIDE
          ))
      }
    }

  private def javaMathConst[Ctxt, Const <: Command.WithResult[Expression]](constName: String): Understands[Ctxt, Const] =
    new Understands[Ctxt, Const] {
      def perform(
        context: Ctxt,
        command: Const
      ): (Ctxt, Expression) = {
        (context, new FieldAccessExpr(mathExp, constName))
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
        if (!context.resolver.resolverInfo.contains(RealArithmeticEnabled)) {
          val resolverUpdate =
            ContextSpecificResolver.updateResolver(base.config, rep, targetType)(reification)(_)
          (context.copy(resolver = resolverUpdate(context.resolver).addInfo(RealArithmeticEnabled)), ())
        } else (context, ())
      }
    })
}