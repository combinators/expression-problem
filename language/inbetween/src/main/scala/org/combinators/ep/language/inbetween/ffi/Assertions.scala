package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Assert, Assertions as Asrts}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

// cannot find 'assertions'
trait Assertions[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: AssertionOps.Factory[FT]] extends Asrts[any.Method[FT]] {
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
  type WithBase[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: AssertionOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Assertions[FT, FactoryType] { val base: B }
  def apply[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: AssertionOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Assertions[FT, FactoryType] {
    val base: _base.type = _base
  }
}

object AssertionOps {
  trait AssertTrueOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def assertTrueOp(): AssertTrueOp[FT]

    def assertTrue(exp: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(assertTrueOp(), exp)
  }
}