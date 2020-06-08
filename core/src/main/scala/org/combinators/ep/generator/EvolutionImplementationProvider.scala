package org.combinators.ep.generator

import cats.free.Free
import cats.kernel.Monoid
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.ReceivedRequest
import shapeless.TypeCase

/** Instances of this class provide the domain dependent implementation of an evolution. */
trait EvolutionImplementationProvider[-AIP <: ApproachImplementationProvider] {

  val model: GenericModel

  /** Initializes the project context to support this evolution, e.g. by calling
    * [[org.combinators.ep.generator.paradigm.ffi.FFI.enable()]] for all the required FFIs.
    */
  def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit]

  /** For more complicated ExtensionGraphs, this returns most appropriate evolution for which an EIP is available.
   * In linear histories, this is the most recent. For histories involving merging, the EIP is responsible
   * for choosing which branch(es) to forward request to. Takes care to check against "current model" to
   * avoid returning an implementation for the future. */
  def applicableIn(forApproach: AIP)(onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression], currentModel:GenericModel): Option[GenericModel] =
    if ((model == currentModel || model.before(currentModel)) && applicable(forApproach)(onRequest)) {
      Some(model)
    } else {
      None
    }

  /** Tests if this evolution implementation provider is applicable for the given request */
  def applicable(forApproach: AIP)(onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean

  /** Can vary by operation and data type. */
  def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = Set.empty

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
        override val model = ???  // won't ever access
        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = Command.skip
        override def applicableIn
           (forApproach: AIP)
           (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression], current:GenericModel): Option[GenericModel] = None
        def applicable
           (forApproach: AIP)
           (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = false
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

        // bias is to use the first
        override val model = first.model

        /** Ensure dependencies are union'd through composition. */
        override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
          first.dependencies(op, dt) ++ second.dependencies(op, dt)
        }

        override def applicableIn
          (forApproach: AIP)
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression],currentModel:GenericModel): Option[GenericModel] =
               first.applicableIn(forApproach)(onRequest,currentModel).map(Some(_)).getOrElse(second.applicableIn(forApproach)(onRequest,currentModel))

        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = {
          for {
            _ <- first.initialize(forApproach)
            _ <- second.initialize(forApproach)
          } yield ()
        }

        def applicable
          (forApproach: AIP)
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean =
          first.applicable(forApproach)(onRequest) || second.applicable(forApproach)(onRequest)

        def logic
            (forApproach: AIP)
            (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
          if (first.applicable(forApproach)(onRequest)) {
            first.logic(forApproach)(onRequest)
          } else {
            second.logic(forApproach)(onRequest)
          }
        }
      }
    }
}
