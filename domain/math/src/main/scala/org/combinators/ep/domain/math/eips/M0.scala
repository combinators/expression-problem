package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}

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
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
       ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
    EvolutionImplementationProvider[AIP[paradigm.type]] =
    new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M0.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()      // need strings BECAUSE test cases require empty string for messages.
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if ((potentialRequest.op == math.M0.Eval) && Set(math.M0.Add, math.M0.Lit).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }
//
//      def applicable (forApproach: AIP[paradigm.type], onRequest:PotentialRequest): Boolean = {
//        (onRequest.op == math.M0.Eval) && Set(math.M0.Add, math.M0.Lit).contains(onRequest.tpeCase)
//      }

      override def logic
          (forApproach: AIP[paradigm.type ])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._
        import paradigm._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty, "failed on " + onRequest.tpeCase.name + " for " + onRequest.request.op.name)
        val result = onRequest.tpeCase match {

          /** Get and return first (and only) attribute. */
          case litC@math.M0.Lit => Command.lift[MethodBodyContext, paradigm.syntax.Expression](onRequest.attributes(litC.attributes.head))

          /** Need to dispatch 'eval' to the left and right. */
          case addC@math.M0.Add =>
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(addC.attributes.head),
                math.M0.getModel.baseDataType,
                onRequest.request
              ))
              right <- forApproach.dispatch(SendRequest(
                onRequest.attributes(addC.attributes.tail.head),
                math.M0.getModel.baseDataType,
                onRequest.request
              ))

              res <- add(left, right)    // FFI capability
            } yield res

          case _ => ???
        }
        result.map(Some(_))
      }
    }
}