package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Abs,  Cos, EulersNumber, Floor, Log, Max, Min, Pi, Pow, Sin, Sqrt, RealArithmetic as RealArith}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait RealArithmetic[AST <: RealArithmeticAST, B, T](val _base: AnyParadigm.WithAST[AST] & B) {
  trait RealArithmeticIn[Ctxt] extends RealArith[Ctxt, T] {
    val base: _base.type = _base

    import base.ast.realArithmeticOpsFactory
    import base.ast.any

    val realArithmeticCapabilities: RealArithmeticCapabilities = new RealArithmeticCapabilities {
      implicit val canSqrt: Understands[Ctxt, Apply[Sqrt[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Sqrt[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Sqrt[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.sqrt(command.arguments.head))
          }
        }
      implicit val canPow: Understands[Ctxt, Apply[Pow[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Pow[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Pow[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.pow(command.arguments.head, command.arguments.tail.head))
          }
        }
      implicit val canLog: Understands[Ctxt, Apply[Log[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Log[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Log[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.log(command.arguments(0), command.arguments(1)))
          }
        }

      implicit val canMax: Understands[Ctxt, Apply[Max[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Max[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Max[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.max(command.arguments(0), command.arguments(1)))
          }
        }
        
      implicit val canMin: Understands[Ctxt, Apply[Min[T], any.Expression, any.Expression]] =
          new Understands[Ctxt, Apply[Min[T], any.Expression, any.Expression]] {
            def perform(context: Ctxt, command: Apply[Min[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
              (context, realArithmeticOpsFactory.min(command.arguments(0), command.arguments(1)))
            }
          }
        
      implicit val canSin: Understands[Ctxt, Apply[Sin[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Sin[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Sin[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.sin(command.arguments.head))
          }
        }
      implicit val canCos: Understands[Ctxt, Apply[Cos[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Cos[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Cos[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.cos(command.arguments.head))
          }
        }
      implicit val canEuler: Understands[Ctxt, EulersNumber[any.Expression]] =
        new Understands[Ctxt, EulersNumber[any.Expression]] {
          def perform(context: Ctxt, command: EulersNumber[any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.eulersNumber())
          }
        }
      implicit val canPi: Understands[Ctxt, Pi[any.Expression]] =
        new Understands[Ctxt, Pi[any.Expression]] {
          def perform(context: Ctxt, command: Pi[any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.pi())
          }
        }
      implicit val canAbs: Understands[Ctxt, Apply[Abs[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Abs[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Abs[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.abs(command.arguments.head))
          }
        }
      implicit val canFloor: Understands[Ctxt, Apply[Floor[T], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Floor[T], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Floor[T], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, realArithmeticOpsFactory.floor(command.arguments.head))
          }
        }
    }
  }
}
object RealArithmetic {
  type WithBase[T, AST <: RealArithmeticAST, B <: AnyParadigm.WithAST[AST]] = RealArithmetic[AST, B, T] {val _base: B}

  def apply[T, AST <: RealArithmeticAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[T, AST, B] = new RealArithmetic[AST, B, T](_base) {}
}
