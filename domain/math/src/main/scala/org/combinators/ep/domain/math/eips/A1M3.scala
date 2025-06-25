package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.Strings
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}

object A1M3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
  a1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type])
  :
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val a1m3_provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.A1M3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiStrings.enable()
          _ <- m3Provider.initialize(forApproach)
          _ <- a1Provider.initialize(forApproach)
        } yield ()
      }

      // A1:
      // M3:

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (Set(math.systemI.I1.MultBy).contains(potentialRequest.op) && Set(math.M3.Mult, math.M3.Neg, math.M3.Divd).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }


      // NEED this since I have stated I will handle some of these
      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import AnyParadigm.syntax._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case mb@math.systemI.I1.MultBy =>      // take advantage of Mult data type
              for {
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
              } yield res

            case _ => ???
          }

        val result =
          for {
            atts <- forEach (onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M3.getModel.baseDataType,
                onRequest.request
              ))
            }
            res <- operate(atts)
          } yield res

        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(a1m3_provider, monoidInstance.combine(a1Provider, m3Provider))
  }
}
