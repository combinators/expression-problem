package org.combinators.dp.original

import org.combinators.cogen.Command.*
import org.combinators.cogen.NameProvider
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.models.original.Model

/** Attempt to provide a dynamic programming world generator. */
trait DPProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  import paradigm.*

  /** Entry point into code generation. */
  def implement(model:Model, option:GenerationOption): Generator[ProjectContext, Unit]

}

