package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.ffi.Equality
import org.combinators.cogen.paradigm.{Apply, ffi}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.cogen.paradigm.ffi
import org.combinators.cogen.paradigm.ffi.{Equality as Eqls, *}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.cogen.Command


trait Equals[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: EqualsOps.Factory[FT]] extends Equality[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory

  val equalityCapabilities: EqualityCapabilities = new EqualityCapabilities {
    implicit val canEquals: Understands[any.Method[FT], Apply[ffi.Equals[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] = new Understands[any.Method[FT], Apply[ffi.Equals[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] {
      def perform(context: any.Method[FT], command: Apply[ffi.Equals[any.Type[FT]], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
        (context, factory.equals(command.functional.inType, command.arguments.head, command.arguments.tail.head))
      }
    }
  }
  def enable(): Generator[any.Project[FT], Unit] = Command.skip[any.Project[FT]]
}

object Equals {
  type WithBase[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: EqualsOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Equals[FT, FactoryType] { val base: B }
  def apply[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: EqualsOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Equals[FT, FactoryType] {
    val base: _base.type = _base
  }
}

object EqualsOps {
  trait Equals[FT <: any.FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def tpe: any.Type[FT]
    def left: any.Expression[FT]
    def right: any.Expression[FT]

    def copy(
      tpe: any.Type[FT],
      left: any.Expression[FT] = left,
      right: any.Expression[FT] = right
    ): Equals[FT] = equals(tpe, left, right)
  }

  trait Factory[FT <: any.FinalTypes] extends any.Factory[FT] {
    def equals(tpe: any.Type[FT], left: any.Expression[FT], right: any.Expression[FT]): Equals[FT]
  }
}