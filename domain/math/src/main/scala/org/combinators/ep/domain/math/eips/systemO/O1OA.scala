package org.combinators.ep.domain.math.eips.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}

object O1OA {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (o1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   oaProvider: EvolutionImplementationProvider[AIP[paradigm.type]])
  :
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val o1oa_provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemO.O1OA.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- o1Provider.initialize(forApproach)
          _ <- oaProvider.initialize(forApproach)
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
    monoidInstance.combine(o1oa_provider, monoidInstance.combine(o1Provider, oaProvider))
  }
}
