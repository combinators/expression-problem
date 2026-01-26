package org.combinators.dp

import org.combinators.ep.generator.Command._
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.model.Model

/** Attempt to provide a dynamic programming world generator. */
trait DPProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  import paradigm._

  /** Entry point into code generation. */
  def implement(model:Model, option:GenerationOption): Generator[ProjectContext, Unit]
}

