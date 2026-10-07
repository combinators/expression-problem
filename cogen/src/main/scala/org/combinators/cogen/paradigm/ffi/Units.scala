package org.combinators.cogen.paradigm.ffi     /*DI:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm

trait Units[Context] extends FFI {

  trait UnitCapabilities {
   
  }
  val unitCapabilities: UnitCapabilities
}

object Units {
  type WithBase[Ctxt, B <: AnyParadigm] = Units[Ctxt] { val base: B }
}