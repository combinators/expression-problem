package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Add, Div, LE, LT, Mod, Mult, Sub, Arithmetic => Arith}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

class Arithmetic[FT <: operatorExpression.FinalTypes, FactoryType <: arithmetic.Factory[FT], T](val base: AnyParadigm[FT, FactoryType]) extends Arith[any.Method[FT], T] {
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
