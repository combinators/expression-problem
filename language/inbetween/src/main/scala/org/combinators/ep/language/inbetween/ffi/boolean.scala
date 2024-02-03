package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Booleans => Bools, _}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.generator.Command

// cannot find 'boolean'
trait Booleans[FT <: operatorExpression.FinalTypes, FactoryType <: boolean.Factory[FT]] extends Bools[any.Method[FT]] {
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
  type WithBase[FT <: operatorExpression.FinalTypes, FactoryType <: boolean.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Booleans[FT, FactoryType] { val base: B }
  def apply[FT <: operatorExpression.FinalTypes, FactoryType <: boolean.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Booleans[FT, FactoryType] {
    val base: _base.type = _base
  }
}