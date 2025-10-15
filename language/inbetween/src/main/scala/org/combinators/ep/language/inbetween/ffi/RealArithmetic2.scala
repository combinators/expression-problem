package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Abs, Add, Cos, Div, EulersNumber, Floor, LE, LT, Log, Mod, Mult, Pi, Pow, Sin, Sqrt, Sub, RealArithmetic as RealArith}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}

// cannot find 'realArithmetic'
trait RealArithmetic2[T](val _base: AnyParadigm2.WithAST[RealArithmeticAST]) {
  trait RealArithmeticInMethods extends RealArith[_base.ast.any.Method, T] {
    val base: _base.type = _base

    import base.ast.realArithmeticOpsFactory
    import base.ast.any

    val realArithmeticCapabilities: RealArithmeticCapabilities = new RealArithmeticCapabilities {
      implicit val canSqrt: Understands[any.Method, Apply[Sqrt[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Sqrt[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Sqrt[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.sqrt(command.arguments.head))
          }
        }
      implicit val canPow: Understands[any.Method, Apply[Pow[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Pow[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Pow[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.pow(command.arguments.head, command.arguments.tail.head))
          }
        }
      implicit val canLog: Understands[any.Method, Apply[Log[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Log[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Log[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.log(command.arguments(0), command.arguments(1)))
          }
        }
      implicit val canSin: Understands[any.Method, Apply[Sin[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Sin[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Sin[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.sin(command.arguments.head))
          }
        }
      implicit val canCos: Understands[any.Method, Apply[Cos[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Cos[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Cos[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.cos(command.arguments.head))
          }
        }
      implicit val canEuler: Understands[any.Method, EulersNumber[any.Expression]] =
        new Understands[any.Method, EulersNumber[any.Expression]] {
          def perform(context: any.Method, command: EulersNumber[any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.eulersNumber())
          }
        }
      implicit val canPi: Understands[any.Method, Pi[any.Expression]] =
        new Understands[any.Method, Pi[any.Expression]] {
          def perform(context: any.Method, command: Pi[any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.pi())
          }
        }
      implicit val canAbs: Understands[any.Method, Apply[Abs[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Abs[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Abs[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.abs(command.arguments.head))
          }
        }
      implicit val canFloor: Understands[any.Method, Apply[Floor[T], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Floor[T], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Floor[T], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, realArithmeticOpsFactory.floor(command.arguments.head))
          }
        }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
  
  val realArithmeticInMethods: RealArithmeticInMethods = new RealArithmeticInMethods {}
}
object RealArithmetic2 {
  type WithBase[T, AST <: RealArithmeticAST, B <: AnyParadigm2.WithAST[AST]] = RealArithmetic2[T] {val _base: B}

  trait WB[T, AST <: RealArithmeticAST, B <: AnyParadigm2.WithAST[AST]](override val _base: B) extends RealArithmetic2[T] {}

  def apply[T, AST <: RealArithmeticAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[T, AST, _base.type] = new WB[T, AST, _base.type](_base) with RealArithmetic2[T](_base) {}
}
