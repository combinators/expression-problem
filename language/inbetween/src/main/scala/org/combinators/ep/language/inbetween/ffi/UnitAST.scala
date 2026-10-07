package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.any.AnyAST

trait UnitAST extends AnyAST  {
  object unitOps {

    trait Unit extends any.Expression

    trait Factory {
      def unitExp(): Unit
    }
  }
  
  val unitOpsFactory: unitOps.Factory
}