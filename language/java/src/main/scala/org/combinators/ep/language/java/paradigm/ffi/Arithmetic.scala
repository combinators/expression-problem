package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.expr.BinaryExpr
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Add, Div, LE, LT, Mod, Mult, Sub, Arithmetic => Arith}
import org.combinators.ep.language.java.paradigm.AnyParadigm
import org.combinators.ep.language.java.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.CodeGenerator.Enable

class Arithmetic[Ctxt, T, AP <: AnyParadigm](
  val base: AP,
  rep: TypeRep.OfHostType[T],
  targetType: Type,
  reification: T => Expression
) extends Arith[Ctxt, T] {
  import org.combinators.ep.language.java.OperatorExprs._

  val arithmeticCapabilities: ArithmeticCapabilities =
    new ArithmeticCapabilities {
      implicit val canAdd: Understands[Ctxt, Apply[Add[T], Expression, Expression]] =
        infixExprOp(BinaryExpr.Operator.PLUS)
      implicit val canSub: Understands[Ctxt, Apply[Sub[T], Expression, Expression]] =
        infixExprOp(BinaryExpr.Operator.MINUS)
      implicit val canMult: Understands[Ctxt, Apply[Mult[T], Expression, Expression]] =
        infixExprOp(BinaryExpr.Operator.MULTIPLY)
      implicit val canDiv: Understands[Ctxt, Apply[Div[T], Expression, Expression]] =
        infixExprOp(BinaryExpr.Operator.DIVIDE)
      implicit val canMod: Understands[Ctxt, Apply[Mod[T], Expression, Expression]] =
        infixExprOp(BinaryExpr.Operator.REMAINDER)
      implicit val canLT: Understands[Ctxt, Apply[LT[T], Expression, Expression]] =
        infixExprOp(BinaryExpr.Operator.LESS)
      implicit val canLE: Understands[Ctxt, Apply[LE[T], Expression, Expression]] =
        infixExprOp(BinaryExpr.Operator.LESS_EQUALS)
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