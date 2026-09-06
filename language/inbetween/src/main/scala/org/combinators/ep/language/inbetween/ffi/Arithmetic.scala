package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Add, Div, LE, LT, Mod, Mult, Sub, Arithmetic as Arith}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Arithmetic[AST <: ArithmeticAST, B, T](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ArithmeticIn[Ctxt] extends Arith[Ctxt, T] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.arithmeticOpsFactory
    override val base: _base.type = _base

    val arithmeticCapabilities: ArithmeticCapabilities = new ArithmeticCapabilities {
      implicit val canLT: Understands[Ctxt, Apply[LT[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[LT[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[LT[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arithmeticOpsFactory.lt(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canLE: Understands[Ctxt, Apply[LE[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[LE[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[LE[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arithmeticOpsFactory.le(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canAdd: Understands[Ctxt, Apply[Add[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Add[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Add[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arithmeticOpsFactory.add(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canSub: Understands[Ctxt, Apply[Sub[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Sub[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Sub[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arithmeticOpsFactory.sub(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canMult: Understands[Ctxt, Apply[Mult[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Mult[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Mult[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arithmeticOpsFactory.mult(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canDiv: Understands[Ctxt, Apply[Div[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Div[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Div[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arithmeticOpsFactory.div(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canMod: Understands[Ctxt, Apply[Mod[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Mod[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Mod[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arithmeticOpsFactory.mod(command.arguments(0), command.arguments(1)))
          }
        }
    }
  }
}
object Arithmetic {
  type WithBase[T, AST <: ArithmeticAST, B <: AnyParadigm.WithAST[AST]] = Arithmetic[AST, B, T] {}
  def apply[T, AST <: ArithmeticAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[T, AST, B] = new Arithmetic[AST, B, T](_base) {}
}
