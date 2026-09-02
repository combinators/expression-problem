package org.combinators.ep.language.inbetween

import org.combinators.cogen.TypeRep
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.FFI

trait ContextRegistry[B <: AnyParadigm, Ctxt](val base: B) {
  def enable(
    ffi: FFI,
    tpeLookup: TypeRep => Option[Generator[Ctxt, base.syntax.Type]],
    reifylookup: (typeRep: TypeRep) => typeRep.HostType => Option[Generator[Ctxt, base.syntax.Expression]]
  ): Generator[base.ProjectContext, Unit]
}
