package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.Arithmetic
import EvolutionImplementationProvider._

object M1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (m0Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]):
    EvolutionImplementationProvider[AIP[paradigm.type]] = {

    val m1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        ffiArithmetic.enable()
      }

      def applicable
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (onRequest.request.op == math.M0.Eval) &&
          (Set(math.M1.Sub).contains(onRequest.tpeCase))
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        m0Provider.genericLogic(forApproach)(onRequest)

      override def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._

        assert(applicable(forApproach)(onRequest))

        val result = onRequest.tpeCase match {
          case subC@math.M1.Sub =>
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(subC.attributes.head),
                math.M1.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
              right <- forApproach.dispatch(SendRequest(
                onRequest.attributes(subC.attributes.tail.head),
                math.M1.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
              res <- sub(left, right)
            } yield res
          case _ => ???
        }
        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(m1Provider, m0Provider)
  }
}
