package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.any

object assertions {
  trait AssertTrueOp[FT <: operatorExpression.FinalTypes] extends operatorExpression.Operator[FT]

  trait Factory[FT <: operatorExpression.FinalTypes] extends operatorExpression.Factory[FT] {
    def assertTrueOp(): AssertTrueOp[FT]

    def assertTrue(exp: any.Expression[FT]): operatorExpression.UnaryExpression[FT] =
      unaryExpression(assertTrueOp(), exp)
  }
}