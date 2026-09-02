package org.combinators.ep.language.inbetween.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, ffi}
import org.combinators.ep.language.inbetween.ContextRegistry


trait FFI[Ctxt] extends ffi.FFI {
  val registry: ContextRegistry[base.type, Ctxt]
  val tpeLookup: TypeRep => Option[Generator[Ctxt, base.syntax.Type]]
  val reifylookup: (typeRep: TypeRep) => typeRep.HostType => Option[Generator[Ctxt, base.syntax.Expression]]

  override def enable(): Generator[base.ProjectContext, Unit] = {
    for {
      _ <- registry.enable(this, tpeLookup, reifylookup)
    } yield ()
  }
}
