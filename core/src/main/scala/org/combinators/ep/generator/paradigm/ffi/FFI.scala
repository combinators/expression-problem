package org.combinators.ep.generator.paradigm.ffi

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm

trait FFI {
  val base: AnyParadigm
  def enable(): Generator[base.ProjectContext, Unit]
}
