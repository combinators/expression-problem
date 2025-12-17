package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Add, Div, LE, LT, Mod, Mult, Sub, Arithmetic as Arith}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}


trait Arithmetic2[AST <: ArithmeticOpsAST, B, T](val _base: AnyParadigm2.WithAST[AST] & B) {
  trait ArithmeticInMethods extends Arith[_base.ast.any.Method, T] {
    override val base: _base.type = _base

    import base.ast.any
    import base.ast.arithmeticOpsFactory

    val arithmeticCapabilities: ArithmeticCapabilities = new ArithmeticCapabilities {
      implicit val canLT: Understands[any.Method, Apply[LT[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[LT[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[LT[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arithmeticOpsFactory.lt(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canLE: Understands[any.Method, Apply[LE[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[LE[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[LE[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arithmeticOpsFactory.le(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canAdd: Understands[any.Method, Apply[Add[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Add[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Add[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arithmeticOpsFactory.add(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canSub: Understands[any.Method, Apply[Sub[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Sub[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Sub[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arithmeticOpsFactory.sub(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canMult: Understands[any.Method, Apply[Mult[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Mult[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Mult[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arithmeticOpsFactory.mult(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canDiv: Understands[any.Method, Apply[Div[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Div[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Div[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arithmeticOpsFactory.div(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canMod: Understands[any.Method, Apply[Mod[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Mod[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Mod[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arithmeticOpsFactory.mod(command.arguments(0), command.arguments(1)))
          }
        }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }

  val arithmeticInMethods: ArithmeticInMethods = new ArithmeticInMethods {}
}
object Arithmetic2 {
  type WithBase[T, AST <: ArithmeticOpsAST, B <: AnyParadigm2.WithAST[AST]] = Arithmetic2[AST, B, T] {}
  def apply[T, AST <: ArithmeticOpsAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[T, AST, B] = new Arithmetic2[AST, B, T](_base) {}
}
