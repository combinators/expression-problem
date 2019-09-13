package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.Arithmetic
import EvolutionImplementationProvider._

object M1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type]):
    EvolutionImplementationProvider[AIP[paradigm.type]] = {

    val subProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        ffiArithmetic.enable()
      }

      override def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._

        assert(onRequest.request.op == math.M0.Eval)

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
    monoidInstance.combine(subProvider, M0(paradigm)(ffiArithmetic))
  }
}
