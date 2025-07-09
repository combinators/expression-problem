package org.combinators.ep.language.java     /*DI:LD:AI*/

import com.github.javaparser.ast.expr.{BinaryExpr, EnclosedExpr, Expression, UnaryExpr}
import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.Understands

/**
 * Operator Expressions are enclosed with parens (EnclosedExpr) to ensure correctness, even though this might
 * lead to extra parens. These could always be filtered out later by a simplifier.
 */
object OperatorExprs {
  def infixExprOp[Ctxt, Op](infixOp: BinaryExpr.Operator): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, new EnclosedExpr(new BinaryExpr(command.arguments(0), command.arguments(1), infixOp)))
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
        import scala.reflect.Selectable.reflectiveSelectable
        if (command.functional.shortcut) {
          (context, new EnclosedExpr (new BinaryExpr(command.arguments(0), command.arguments(1), shortCutOp)))
        } else {
          (context, new EnclosedExpr (new BinaryExpr(command.arguments(0), command.arguments(1), normalOp)))
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
