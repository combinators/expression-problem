package org.combinators.dp.enhanced

import org.combinators.dp.TestExample
import org.combinators.cogen.Command._
import org.combinators.cogen.NameProvider
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.dp.original.GenerationOption
import org.combinators.models.EnhancedModel

/** Attempt to provide a dynamic programming world generator. */
trait EnhancedDPProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  import paradigm._

  /** Entry point into code generation. */
  def implement(model:EnhancedModel, tests:Seq[TestExample], option:GenerationOption): Generator[ProjectContext, Unit]
}
