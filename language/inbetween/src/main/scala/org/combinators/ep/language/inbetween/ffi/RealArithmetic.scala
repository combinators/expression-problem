package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Abs, Add, Cos, Div, EulersNumber, Floor, LE, LT, Log, Mod, Mult, Pi, Pow, Sin, Sqrt, Sub, RealArithmetic => RealArith}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

// cannot find 'realArithmetic'
trait RealArithmetic[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: RealArithmeticOps.Factory[FT], T] extends RealArith[any.Method[FT], T] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory
  val realArithmeticCapabilities: RealArithmeticCapabilities = new RealArithmeticCapabilities {
    implicit val canSqrt: Understands[any.Method[FT], Apply[Sqrt[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Sqrt[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Sqrt[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.sqrt(command.arguments.head))
        }
      }
    implicit val canPow: Understands[any.Method[FT], Apply[Pow[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Pow[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Pow[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.pow(command.arguments.head, command.arguments.tail.head))
        }
      }
    implicit val canLog: Understands[any.Method[FT], Apply[Log[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Log[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Log[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.log(command.arguments(0), command.arguments(1)))
        }
      }
    implicit val canSin: Understands[any.Method[FT], Apply[Sin[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Sin[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Sin[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.sin(command.arguments.head))
        }
      }
    implicit val canCos: Understands[any.Method[FT], Apply[Cos[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Cos[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Cos[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.cos(command.arguments.head))
        }
      }
    implicit val canEuler: Understands[any.Method[FT], EulersNumber[any.Expression[FT]]] =
      new Understands[any.Method[FT], EulersNumber[any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: EulersNumber[any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.eulersNumber())
        }
      }
    implicit val canPi: Understands[any.Method[FT], Pi[any.Expression[FT]]] =
      new Understands[any.Method[FT], Pi[any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Pi[any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.pi())
        }
      }
    implicit val canAbs: Understands[any.Method[FT], Apply[Abs[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Abs[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Abs[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.abs(command.arguments.head))
        }
      }
    implicit val canFloor: Understands[any.Method[FT], Apply[Floor[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Floor[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Floor[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.floor(command.arguments.head))
        }
      }
  }
  def enable(): Generator[any.Project[FT], Unit] = Command.skip[any.Project[FT]]
}
object RealArithmetic {
  type WithBase[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: RealArithmeticOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType], T] = RealArithmetic[FT, FactoryType, T] { val base: B }
  def apply[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: RealArithmeticOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType], T](_base: B): WithBase[FT, FactoryType, _base.type, T] = new RealArithmetic[FT, FactoryType, T] {
    val base: _base.type = _base
  }
}

object RealArithmeticOps {
  trait SqrtOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait PowOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait LogOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait SinOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait CosOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait AbsOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait FloorOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait EulersNumber[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]

  trait Pi[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]


  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def sqrtOp(): SqrtOp[FT]
    def powOp(): PowOp[FT]
    def logOp(): LogOp[FT]
    def sinOp(): SinOp[FT]
    def cosOp(): CosOp[FT]
    def absOp(): AbsOp[FT]
    def floorOp(): FloorOp[FT]

    def pi(): Pi[FT]
    def eulersNumber(): EulersNumber[FT]

    def sqrt(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(sqrtOp(), of)
    def pow(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(powOp(), left, right)

    def log(of: any.Expression[FT], base: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(logOp(), of, base) // FIXME: unaryExpression(logOp(), of)
    def sin(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(sinOp(), of)
    def cos(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(cosOp(), of)
    def abs(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(absOp(), of)
    def floor(of: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(floorOp(), of)
  }
}