package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Assert, Assertions => Asrts}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Assertions[FT <: operatorExpression.FinalTypes, FactoryType <: assertions.Factory[FT]] extends Asrts[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
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

object Assertions {
  type WithBase[FT <: operatorExpression.FinalTypes, FactoryType <: assertions.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Assertions[FT, FactoryType] { val base: B }
  def apply[FT <: operatorExpression.FinalTypes, FactoryType <: assertions.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Assertions[FT, FactoryType] {
    val base: _base.type = _base
  }
}