package org.combinators.cogen.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.Command.Generator

/**
 * Foreign Function Interfaces.
 *
 * Initialization is contained in the enable method.
 */
trait FFI {
  val base: AnyParadigm
  def enable(): Generator[base.ProjectContext, Unit]
}
