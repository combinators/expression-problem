package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object D2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (m1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
       ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val d2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.D2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
        } yield ()
      }

      def applicable
        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        Set(math.M0.Eval).contains(potentialRequest.op) &&
          Set(math.D2.Mult).contains(potentialRequest.tpeCase)
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (Set(math.M0.Eval).contains(potentialRequest.op) && Set(math.D2.Mult).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        m1Provider.genericLogic(forApproach)(onRequest)

      def logic
          (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty, onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.D2.Mult => mult(atts: _*)

                case _ => ???
              }

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

    // newest one must come first
    monoidInstance.combine(d2Provider, m1Provider)
  }
}
