package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{And, False, Not, Or, True, Booleans as Bools}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Booleans[AST <: BooleanAST, B, T](val _base: AnyParadigm.WithAST[AST] & B) {
  trait BooleansIn[Ctxt] extends Bools[Ctxt, T] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.booleanOpsFactory
    override val base: _base.type = _base

    val booleanCapabilities: BooleanCapabilities =
      new BooleanCapabilities {
        implicit val canAnd: Understands[Ctxt, Apply[And, any.Expression, any.Expression]] = new Understands[Ctxt, Apply[And, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[And, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            val andExp = if (command.arguments.isEmpty) {
              booleanOpsFactory.falseExp()
            } else {
              command.arguments.reverse.tail.foldRight(command.arguments.reverse.head) { case (s, arg) => booleanOpsFactory.and(arg, s) }
            }
            (context, andExp)
          }
        }
        implicit val canOr: Understands[Ctxt, Apply[Or, any.Expression, any.Expression]] = new Understands[Ctxt, Apply[Or, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Or, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            val orExp = if (command.arguments.isEmpty) {
              booleanOpsFactory.trueExp()
            } else {
              command.arguments.reverse.tail.foldRight(command.arguments.reverse.head) { case (s, arg) => booleanOpsFactory.or(arg, s) }
            }
            (context, orExp)
          }
        }
        implicit val canNot: Understands[Ctxt, Apply[Not, any.Expression, any.Expression]] = new Understands[Ctxt, Apply[Not, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Not, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, booleanOpsFactory.not(command.arguments.head))
          }
        }
        implicit val canTrue: Understands[Ctxt, True[any.Expression]] = new Understands[Ctxt, True[any.Expression]] {
          def perform(context: Ctxt, command: True[any.Expression]): (Ctxt, any.Expression) = {
            (context, booleanOpsFactory.trueExp())
          }
        }
        implicit val canFalse: Understands[Ctxt, False[any.Expression]] = new Understands[Ctxt, False[any.Expression]] {
          def perform(context: Ctxt, command: False[any.Expression]): (Ctxt, any.Expression) = {
            (context, booleanOpsFactory.falseExp())
          }
        }
      }
  }
}

object Booleans {
  type WithBase[T, AST <: BooleanAST, B <: AnyParadigm.WithAST[AST]] = Booleans[AST, B, T] {}

  def apply[T, AST <: BooleanAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[T, AST, B] = new Booleans[AST, B, T](_base) {}
}
