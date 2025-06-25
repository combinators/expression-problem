package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Add, Div, LE, LT, Mod, Mult, Sub, Arithmetic as Arith}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

// cannot find 'arithmetic'
trait Arithmetic[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: ArithmeticOps.Factory[FT], T] extends Arith[any.Method[FT], T] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory
  val arithmeticCapabilities: ArithmeticCapabilities = new ArithmeticCapabilities {
    implicit val canLT: Understands[any.Method[FT], Apply[LT[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[LT[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[LT[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.lt(command.arguments(0), command.arguments(1)))
        }
      }
    implicit val canLE: Understands[any.Method[FT], Apply[LE[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[LE[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[LE[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.le(command.arguments(0), command.arguments(1)))
        }
      }
    implicit val canAdd: Understands[any.Method[FT], Apply[Add[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Add[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Add[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.add(command.arguments(0), command.arguments(1)))
        }
      }
    implicit val canSub: Understands[any.Method[FT], Apply[Sub[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Sub[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Sub[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.sub(command.arguments(0), command.arguments(1)))
        }
      }
    implicit val canMult: Understands[any.Method[FT], Apply[Mult[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Mult[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Mult[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.mult(command.arguments(0), command.arguments(1)))
        }
      }
    implicit val canDiv: Understands[any.Method[FT], Apply[Div[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Div[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Div[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.div(command.arguments(0), command.arguments(1)))
        }
      }
    implicit val canMod: Understands[any.Method[FT], Apply[Mod[T], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Mod[T], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Mod[T], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.mod(command.arguments(0), command.arguments(1)))
        }
      }
  }
  def enable(): Generator[any.Project[FT], Unit] = Command.skip[any.Project[FT]]
}
object Arithmetic {
  type WithBase[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: ArithmeticOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType], T] = Arithmetic[FT, FactoryType, T] { val base: B }
  def apply[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: ArithmeticOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType], T](_base: B): WithBase[FT, FactoryType, _base.type, T] = new Arithmetic[FT, FactoryType, T] {
    val base: _base.type = _base
  }
}

object ArithmeticOps {
  trait AddOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait SubOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait MultOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait DivOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait ModOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait LtOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait LeOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def addOp(): AddOp[FT]
    def subOp(): SubOp[FT]
    def multOp(): MultOp[FT]
    def divOp(): DivOp[FT]
    def modOp(): ModOp[FT]
    def ltOp(): LtOp[FT]
    def leOp(): LeOp[FT]

    def add(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(addOp(), left, right)
    def sub(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(subOp(), left, right)
    def mult(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(multOp(), left, right)
    def div(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(divOp(), left, right)
    def mod(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(modOp(), left, right)
    def lt(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(ltOp(), left, right)
    def le(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(leOp(), left, right)
  }
}