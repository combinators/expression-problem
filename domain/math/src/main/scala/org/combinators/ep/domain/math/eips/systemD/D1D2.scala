package org.combinators.ep.domain.math.eips.systemD    /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemD
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

sealed class D1D2[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (d1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],d2Provider: EvolutionImplementationProvider[AIP[paradigm.type]]):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val d1d2_provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = systemD.D1D2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- d1Provider.initialize(forApproach)
          _ <- d2Provider.initialize(forApproach)
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (Set(math.systemD.D1.MultBy).contains(potentialRequest.op) && Set(math.systemD.D2.Mult, math.M1.Sub, math.M0.Add, math.M0.Lit).contains(potentialRequest.tpeCase)) {
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

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        val result =
          onRequest.request.op match {
            case mb@math.systemD.D1.MultBy => // take advantage of Mult data type
              for {
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.systemD.D2.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
              } yield res

            case _ => ???
          }

        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(d1d2_provider, monoidInstance.combine(d1Provider, d2Provider))
  }
}

object D1D2 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (d1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   d2Provider: EvolutionImplementationProvider[AIP[paradigm.type]]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new D1D2[paradigm.type, AIP, Expression](paradigm)

    mkImpl(d1Provider,d2Provider)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (d1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   d2Provider: EvolutionImplementationProvider[AIP[paradigm.type]]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val mkImpl = new D1D2[paradigm.type, AIP, Unit](paradigm)

    mkImpl(d1Provider,d2Provider)
  }
}