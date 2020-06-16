package org.combinators.ep.generator.paradigm.ffi   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm

/**
 * Foreign Function Interfaces.
 *
 * Initialization is contained in the enable method.
 */
trait FFI {
  val base: AnyParadigm
  def enable(): Generator[base.ProjectContext, Unit]
}
