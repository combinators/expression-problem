package org.combinators.ep.domain.math.eips.systemO    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}

object OA {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (m2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]):
    EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val oaProvider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemO.O1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if ((potentialRequest.op == math.M0.Eval) && Set(math.M0.Lit).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        m2Provider.genericLogic(forApproach)(onRequest)

      def logic
          (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        val result = onRequest.tpeCase match {
          /** Get and return first (and only) attribute. */
          case litC@math.M0.Lit => Command.lift[MethodBodyContext, paradigm.syntax.Expression](onRequest.attributes(litC.attributes.head))

          case _ => ???
        }
        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(oaProvider, m2Provider)
  }
}
