package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object W1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val w1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.W1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m1Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

//      def applicable
//      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
//        Set(math.M0.Eval).contains(potentialRequest.op) &&
//          Set(math.W1.Power).contains(potentialRequest.tpeCase)
//      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        // TODO: dependency fix
        None
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

        import ffiArithmetic.arithmeticCapabilities._
        import ffiRealArithmetic.realArithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        //assert(applicable(forApproach)(onRequest)) TODO: fix assert

        val result = onRequest.tpeCase match {
          case power@math.W1.Power => {
            onRequest.request.op match {
              case eval@math.M0.Eval =>
                for {
                  base <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request
                  ))
                  exponent <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.tail.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request
                  ))
                  res <- pow(base, exponent)
                } yield res

              case _ => ???
            }
          }

          case _ => ???
        }
        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(w1Provider, m1Provider)
  }
}
