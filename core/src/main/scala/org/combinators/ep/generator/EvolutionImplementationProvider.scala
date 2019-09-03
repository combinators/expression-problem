package org.combinators.ep.generator

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.ReceivedRequest
import org.combinators.ep.generator.paradigm.AnyParadigm

/** Instances of this class provide the domain dependent implementation of an evolution. */
abstract class EvolutionImplementationProvider[S <: AbstractSyntax, P <: AnyParadigm[S]] {
  /** Generates the code of request handlers relative to the target language and approach specific code generation
    * logic provided by the given `codeGenerator`. */
  def logic
      (forApproach: ApproachImplementationProvider[S, P])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
    Generator[forApproach.paradigm.MethodBodyContext, forApproach.paradigm.syntax.Expression]

}
