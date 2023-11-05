package org.combinators.ep.domain.math.eips.systemX   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object X2X3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (x2Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   x3Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  :
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val x2x3_provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.systemX.X2X3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- x2Provider.initialize(forApproach)
          _ <- x3Provider.initialize(forApproach)
        } yield ()
      }

      // pure merge, where both X2 and X3 only have a new data type. Nothing to do?
      // X2:  Seq(Times), Seq.empty)
      // X3:  Seq(Divd), Seq.empty)
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        None
      }

      // HACK: THIS CAN ENTIRELY BE REMOVED BUT HOW TO DO SKIP HERE?
      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import paradigm._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {

            case _ => ???
          }

        val result =
          for {
            atts <- forEach(onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M0.getModel.baseDataType,
                onRequest.request
              ))
            }
            res <- operate(atts)
          } yield res

        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(x2x3_provider, monoidInstance.combine(x3Provider, x2Provider))
  }
}
