package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{Apply, ffi}
import org.combinators.ep.generator.paradigm.ffi.{Equality => Eqls, _}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.generator.Command


class Equals[FT <: operatorExpression.FinalTypes, FactoryType <: eqls.Factory[FT]](val base: AnyParadigm[FT, FactoryType]) extends Eqls[any.Method[FT]] {
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
