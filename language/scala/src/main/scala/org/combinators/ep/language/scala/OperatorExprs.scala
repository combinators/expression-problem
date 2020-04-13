package org.combinators.ep.language.scala

import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.Apply

import scala.meta._
import org.combinators.ep.language.scala.Syntax.default._

object OperatorExprs {
  def infixExprOp[Ctxt, Op](infixOp: Term.Name): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, Term.ApplyInfix(
          lhs = command.arguments.head,
          op = infixOp,
          targs = List.empty,
          args = command.arguments.tail.toList
        ))
      }
    }

  def shortCutInfixExprOp[Ctxt, Op <: { val shortcut: Boolean }](
    shortCutOp: Term.Name,
    normalOp: Term.Name
  ): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        import scala.language.reflectiveCalls
        if (command.functional.shortcut) {
          (context, Term.ApplyInfix(
            lhs = command.arguments.head,
            op = shortCutOp,
            targs = List.empty,
            args = command.arguments.tail.toList
          ))
        } else {
          (context, Term.ApplyInfix(
            lhs = command.arguments.head,
            op = normalOp,
            targs = List.empty,
            args = command.arguments.tail.toList
          ))
        }
      }
    }

  def prefixExprOp[Ctxt, Op](infixOp: Term.Name): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, Term.ApplyUnary(
          op = infixOp,
          arg = command.arguments.head
        ))
      }
    }
}
