package org.combinators.dp.enhanced

import org.combinators.dp.{GenerationOption, TestExample}
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.model.EnhancedModel

/** Attempt to provide a dynamic programming world generator. */
trait EnhancedDPProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  import paradigm._

  /** Entry point into code generation. */
  def implement(model:EnhancedModel, tests:Seq[TestExample], option:GenerationOption): Generator[ProjectContext, Unit]

}

