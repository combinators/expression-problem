package org.combinators.equals.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.ffi.FFI
import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{TypeRep, Understands}

trait BaseType[Context] extends FFI {
  
}

object BaseType {
  type WithBase[Ctxt, B <: AnyParadigm] = BaseType[Ctxt] { val base: B }
  
  case object AnyTpe extends TypeRep {
    type OfHostType = Any
  }
}


// ultimately will need to translate this into java.lang.Object (java) 