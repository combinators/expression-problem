package org.combinators.ep.domain.shape.eips      /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, shape}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}

object S1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (s0Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val s1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      val model:GenericModel = shape.S1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- s0Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if ((potentialRequest.op == shape.S0.ContainsPt) && Set(shape.S1.Union).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      override def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._
        import ffiBoolean.booleanCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {
          case op if op == shape.S0.ContainsPt => {
            val result = onRequest.tpeCase match {

              /** Need to dispatch 'containsPt' to the inner after normalizing (x,y) accordingly. */
              case union@shape.S1.Union =>
                for {
                  s1Result <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(shape.S1.s1),
                    shape.S0.getModel.baseDataType,
                    Request(shape.S0.ContainsPt, Map(shape.S0.pointx -> onRequest.request.arguments(shape.S0.pointx),
                      shape.S0.pointy -> onRequest.request.arguments(shape.S0.pointy))) // must be changed in response to translate
                    ))

                  s2Result <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(shape.S1.s2),
                    shape.S0.getModel.baseDataType,
                    Request(shape.S0.ContainsPt, Map(shape.S0.pointx -> onRequest.request.arguments(shape.S0.pointx),
                      shape.S0.pointy -> onRequest.request.arguments(shape.S0.pointy)))
                    ))

                  orRes <- or(Seq(s1Result, s2Result))
                } yield orRes

              // Scala response to ultimately cause runtime exception
              case _ => ???
            }
            result.map(Some(_))
          }
        }
      }
    }
    monoidInstance.combine(s1Provider, s0Provider)
  }
}
