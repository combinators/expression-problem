package org.combinators.ep.generator.paradigm.ffi   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}
import org.combinators.ep.generator.{Command, Understands}

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
      AnyParadigm.capabilitiy(Apply[And, Expression, Expression](And(shortcut), exprs))

    implicit val canOr: Understands[Context, Apply[Or, Expression, Expression]]
    def or(exprs: Seq[Expression], shortcut: Boolean = true): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Or, Expression, Expression](Or(shortcut), exprs))

    implicit val canNot: Understands[Context, Apply[Not, Expression, Expression]]
    def not(expr: Expression): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Apply[Not, Expression, Expression](Not(), Seq(expr)))

    implicit val canTrue: Understands[Context, True[Expression]]
    def trueExp: Generator[Context, Expression] =
      AnyParadigm.capabilitiy(True[Expression]())

    implicit val canFalse: Understands[Context, False[Expression]]
    def falseExp: Generator[Context, Expression] =
      AnyParadigm.capabilitiy(False[Expression]())
  }
  val booleanCapabilities: BooleanCapabilities
}

object Booleans {
  type WithBase[Ctxt, B <: AnyParadigm] = Booleans[Ctxt] { val base: B }
}