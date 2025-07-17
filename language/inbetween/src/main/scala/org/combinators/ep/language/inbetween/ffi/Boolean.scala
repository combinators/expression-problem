package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{And, False, Not, Or, True, Booleans as Bools}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.Understands
import org.combinators.cogen.paradigm.ffi._
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.cogen.Command

// cannot find 'boolean'
trait Booleans[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: BooleanOps.Factory[FT]] extends Bools[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory
  val booleanCapabilities: BooleanCapabilities =
    new BooleanCapabilities {
      implicit val canAnd: Understands[any.Method[FT], Apply[And, any.Expression[FT], any.Expression[FT]]] = new Understands[any.Method[FT], Apply[And, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[And, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          val andExp = if (command.arguments.isEmpty) {
            factory.falseExp()
          } else {
            command.arguments.reverse.tail.foldRight(command.arguments.reverse.head) { case (s, arg) => factory.and(arg, s) }
          }
          (context, andExp)
        }
      }
      implicit val canOr: Understands[any.Method[FT], Apply[Or, any.Expression[FT], any.Expression[FT]]] = new Understands[any.Method[FT], Apply[Or, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Or, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          val orExp = if (command.arguments.isEmpty) { factory.trueExp() } else {
            command.arguments.reverse.tail.foldRight(command.arguments.reverse.head){ case (s, arg) => factory.or(arg, s) }
          }
          (context, orExp)
        }
      }
      implicit val canNot: Understands[any.Method[FT], Apply[Not, any.Expression[FT], any.Expression[FT]]] = new Understands[any.Method[FT], Apply[Not, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Not, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.not(command.arguments.head))
        }
      }
      implicit val canTrue: Understands[any.Method[FT], True[any.Expression[FT]]] = new Understands[any.Method[FT], True[any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: True[any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.trueExp())
        }
      }
      implicit val canFalse: Understands[any.Method[FT], False[any.Expression[FT]]] = new Understands[any.Method[FT], False[any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: False[any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.falseExp())
        }
      }
    }

  override def enable(): Generator[any.Project[FT], Unit] = Command.skip[any.Project[FT]]
}

object Booleans {
  type WithBase[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: BooleanOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Booleans[FT, FactoryType] { val base: B }
  def apply[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: BooleanOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Booleans[FT, FactoryType] {
    val base: _base.type = _base
  }
}

object BooleanOps {
  trait AndOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait OrOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait NotOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait True[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]
  trait False[FT <: OperatorExpressionOps.FinalTypes] extends any.Expression[FT]

  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def andOp(): AndOp[FT]
    def orOp(): OrOp[FT]
    def notOp(): NotOp[FT]

    def trueExp(): True[FT]
    def falseExp(): False[FT]
    def and(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(andOp(), left, right)
    def or(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(orOp(), left, right)
    def not(exp: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(notOp(), exp)
  }
}