package org.combinators.ep.domain.math.eips

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.Arithmetic

/**
 * Truly language independent abstractions.  Since we are in the mathematical domain, the
 * ffi package contains mathematical concerns, which are then completely abstract from the underlying
 * language in which the code generator works.
 *
 * Introducing Lit/Add  and Eval.
 *
 * FFI allows this object to specify
 *
 * Doing this here using FFIs allows us to avoid having to create/duplicate nearly identical specifications
 * just to work with different languages, i.e., Java/e0 and then GJ/e0 and CPP/e0
 *
 * There should be many similarities between Java-generator and CPP-generator. I used a "poor man's version"
 * of modeling for C++ code. Move that content into paradigm
 */
object M0 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]):
    EvolutionImplementationProvider[AIP[paradigm.type]] =
    new EvolutionImplementationProvider[AIP[paradigm.type]] {
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        ffiArithmetic.enable()
      }

      def applicable
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (onRequest.request.op == math.M0.Eval) &&
          (Set(math.M0.Add, math.M0.Lit).contains(onRequest.tpeCase))
      }

      override def logic
          (forApproach: AIP[paradigm.type ])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._
        import Command._
        import paradigm._

        // no need to pass up to the chain since only Eval is known
        assert(applicable(forApproach)(onRequest), "failed on " + onRequest.tpeCase.name + " for " + onRequest.request.op.name)

        val result = onRequest.tpeCase match {

          /**
            * Simple enough to handle directly since we know what to return. Get first attribute and this is
            * ultimately just the expression which is to be returned.
            *
            * result(Java("")) TODO: result is not necessary
            */
          case litC@math.M0.Lit => Command.lift[MethodBodyContext, paradigm.syntax.Expression](onRequest.attributes(litC.attributes.head))

          /** Need to dispatch 'eval' to the left and right. */
          case addC@math.M0.Add =>
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(addC.attributes.head),  // instead use look-up addC.attributes.find(att => att.name)
                math.M0.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)  // being sent in response to Eval
              ))
              right <- forApproach.dispatch(SendRequest(
                onRequest.attributes(addC.attributes.tail.head),
                math.M0.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))

              // FFI capability provided in truly language independent manner
              res <- add(left, right)
            } yield res

          // Scala response to ultimately cause runtime exception
          case _ => ???
        }
        result.map(Some(_))
      }
    }
}