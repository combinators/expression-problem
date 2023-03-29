package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Assert, Assertions => Asrts}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

class Assertions[FT <: operatorExpression.FinalTypes, FactoryType <: assertions.Factory[FT]](val base: AnyParadigm[FT, FactoryType]) extends Asrts[any.Method[FT]] {
  import base.factory

  val assertionCapabilities: AssertionCapabilities = new AssertionCapabilities {
    implicit val canAssert: Understands[any.Method[FT], Apply[Assert, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Assert, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[Assert, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.assertTrue(command.arguments.head))
        }
      }
  }
  def enable(): Generator[any.Project[FT], Unit] =  Command.skip[any.Project[FT]]
}
