package org.combinators.ep.language.java     /*DI:LD:AI*/

import com.github.javaparser.ast.expr.{BinaryExpr, UnaryExpr, Expression}
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.Apply

object OperatorExprs {
  def infixExprOp[Ctxt, Op](infixOp: BinaryExpr.Operator): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, new BinaryExpr(command.arguments(0), command.arguments(1), infixOp))
      }
    }

  def shortCutInfixExprOp[Ctxt, Op <: { val shortcut: Boolean }](
    shortCutOp: BinaryExpr.Operator,
    normalOp: BinaryExpr.Operator
  ): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        import scala.language.reflectiveCalls
        if (command.functional.shortcut) {
          (context, new BinaryExpr(command.arguments(0), command.arguments(1), shortCutOp))
        } else {
          (context, new BinaryExpr(command.arguments(0), command.arguments(1), normalOp))
        }
      }
    }

  def prefixExprOp[Ctxt, Op](infixOp: UnaryExpr.Operator): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, new UnaryExpr(command.arguments(0), infixOp))
      }
    }
}
