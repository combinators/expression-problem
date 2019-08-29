package org.combinators.ep.generator

import org.combinators.ep.generator.communication.ReceivedRequest

/** Instances of this class provide the domain dependent implementation of an evolution. */
abstract class EvolutionImplementationProvider[S <: AbstractSyntax] {
  /** Generates the code of request handlers relative to the target language and approach specific code generation
    * logic provided by the given `codeGenerator`. */
  def logic
      (codeGenerator: DomainIndependentGenerator[S])
      (onRequest: ReceivedRequest[codeGenerator.syntax.Expression]):
    (Seq[codeGenerator.syntax.Import],
      CodeBlockWithResultingExpressions[codeGenerator.syntax.Expression, codeGenerator.syntax.Statement])
}
