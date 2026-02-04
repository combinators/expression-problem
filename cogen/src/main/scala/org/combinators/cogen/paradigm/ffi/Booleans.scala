package org.combinators.cogen.paradigm.ffi

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class And(shortcut: Boolean = true)
case class Or(shortcut: Boolean = true)
case class Not()

case class True[Expression]() extends Command {
  type Result = Expression
}
case class False[Expression]() extends Command {
  type Result = Expression
}

trait Booleans[Context] extends FFI {
  import base.syntax._

  trait BooleanCapabilities {
    implicit val canAnd: Understands[Context, Apply[And, Expression, Expression]]
    def and(exprs: Seq[Expression], shortcut: Boolean = true): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[And, Expression, Expression](And(shortcut), exprs))

    implicit val canOr: Understands[Context, Apply[Or, Expression, Expression]]
    def or(exprs: Seq[Expression], shortcut: Boolean = true): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Or, Expression, Expression](Or(shortcut), exprs))

    implicit val canNot: Understands[Context, Apply[Not, Expression, Expression]]
    def not(expr: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Not, Expression, Expression](Not(), Seq(expr)))

    implicit val canTrue: Understands[Context, True[Expression]]
    def trueExp: Generator[Context, Expression] =
      AnyParadigm.capability(True[Expression]())

    implicit val canFalse: Understands[Context, False[Expression]]
    def falseExp: Generator[Context, Expression] =
      AnyParadigm.capability(False[Expression]())
  }
  val booleanCapabilities: BooleanCapabilities
}

object Booleans {
  type WithBase[Ctxt, B <: AnyParadigm] = Booleans[Ctxt] { val base: B }
}