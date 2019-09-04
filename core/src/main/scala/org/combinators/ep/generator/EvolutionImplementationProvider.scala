package org.combinators.ep.generator

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.ReceivedRequest
import org.combinators.ep.generator.paradigm.AnyParadigm

/** Instances of this class provide the domain dependent implementation of an evolution. */
trait EvolutionImplementationProvider {
  val syntax: AbstractSyntax
  import syntax._

  /** Generates the code of request handlers relative to the target language and approach specific code generation
    * logic provided by the given `codeGenerator`. */
  def logic
      (forApproach: ApproachImplementationProvider.WithSyntax[syntax.type])
      (onRequest: ReceivedRequest[Expression]):
    Generator[forApproach.paradigm.MethodBodyContext, Expression]

}

object EvolutionImplementationProvider {
  type WithSyntax[S] = EvolutionImplementationProvider { val syntax: S }
}