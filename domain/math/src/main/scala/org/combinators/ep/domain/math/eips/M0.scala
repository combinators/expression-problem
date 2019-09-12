package org.combinators.ep.domain.math.eips

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.Arithmetic

object M0 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type]):
    EvolutionImplementationProvider[AIP[paradigm.type]] =
    new EvolutionImplementationProvider[AIP[paradigm.type]] {
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        ffiArithmetic.enable()
      }

      override def logic
          (forApproach: AIP[paradigm.type ])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        import ffiArithmetic.arithmeticCapabilities._
        import Command._
        import paradigm._

        assert(onRequest.request.op == math.M0.Eval)
        val result = onRequest.tpeCase match {
          case litC@math.M0.Lit => Command.lift[MethodBodyContext, paradigm.syntax.Expression](onRequest.attributes(litC.attributes.head))
          case addC@math.M0.Add =>
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(addC.attributes.head),
                math.M0.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
              right <- forApproach.dispatch(SendRequest(
                onRequest.attributes(addC.attributes.tail.head),
                math.M0.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
              res <- add(left, right)
            } yield res
          case _ => ???
        }
        result
      }
    }
}