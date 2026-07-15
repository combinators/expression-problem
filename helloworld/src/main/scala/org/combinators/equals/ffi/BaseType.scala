package org.combinators.equals.ffi

import org.combinators.cogen.paradigm.ffi.FFI
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.TypeRep
import org.combinators.equals.CompositeDataType

trait BaseType[Context] extends FFI {
  
}

object BaseType {
  type WithBase[Ctxt, B <: AnyParadigm] = BaseType[Ctxt] { val base: B }
  
  case object AnyTpe extends TypeRep {
    override type HostType = Any
  }

  case class CompositeTpe(argument:CompositeDataType) extends TypeRep {
    override type HostType = Map[String, Any]
  }
}
