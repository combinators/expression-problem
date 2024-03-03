package org.combinators.ep.domain.math.eips.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}

object OD3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (od1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   od2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  :
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val od3_provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemO.OD3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- od1Provider.initialize(forApproach)
          _ <- od2Provider.initialize(forApproach)
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
       None
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {

        // WON'T BE CALLED BUT NEED SOMETHING
        val result = onRequest.tpeCase match {
          /** Get and return first (and only) attribute. */
          case litC@math.M0.Lit => Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Expression](onRequest.attributes(litC.attributes.head))
          case _ => ???
        }
        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(od3_provider, monoidInstance.combine(od1Provider, od2Provider))
  }
}
