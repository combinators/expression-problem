package org.combinators.ep.generator

import cats.kernel.Monoid
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.ReceivedRequest
import org.combinators.ep.generator.paradigm.AnyParadigm

import scala.util.Try

/** Instances of this class provide the domain dependent implementation of an evolution. */
trait EvolutionImplementationProvider[AIP <: ApproachImplementationProvider] {

  /** Initializes the project context to support this evolution, e.g. by calling
    * [[org.combinators.ep.generator.paradigm.ffi.FFI.enable()]] for all the required FFIs.
    */
  def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit]

  /** Generates the code of request handlers relative to the target language and approach specific code generation
    * logic provided by the given `codeGenerator`. */
  def logic
      (forApproach: AIP)
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
    Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]]
}

object EvolutionImplementationProvider {

  /** Allows to combine multiple [[EvolutionImplementationProvider]] objects into one. */
  implicit def monoidInstance[AIP <: ApproachImplementationProvider]: Monoid[EvolutionImplementationProvider[AIP]] =
    new Monoid[EvolutionImplementationProvider[AIP]] {
      /** Returns an [[EvolutionImplementationProvider]] which does not provide any implementation, and instead fails
        * with a runtime exception */
      def empty: EvolutionImplementationProvider[AIP] = new EvolutionImplementationProvider[AIP] {
        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = Command.skip
        def logic
            (forApproach: AIP)
            (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]) =
          throw new RuntimeException(s"No logic to handle request ${onRequest}")
      }

      /** Combines two [[EvolutionImplementationProvider]] objects by trying to resolve requests with the first provided
        * logic, resorting to the second logic on failure.
        */
      def combine(
          first: EvolutionImplementationProvider[AIP],
          second: EvolutionImplementationProvider[AIP]
        ): EvolutionImplementationProvider[AIP] = new EvolutionImplementationProvider[AIP] {
        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = {
          for {
            _ <- first.initialize(forApproach)
            _ <- second.initialize(forApproach)
          } yield ()
        }

        def logic
            (forApproach: AIP)
            (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]) =
          Try {
            first.logic(forApproach)(onRequest)
          }.getOrElse {
            second.logic(forApproach)(onRequest)
          }
      }
    }

}
