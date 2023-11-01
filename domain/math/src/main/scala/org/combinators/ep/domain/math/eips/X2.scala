package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}

object X2  {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (x1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val x2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.X2.getModel
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

//      def applicable
//      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
//        Set(math.M0.Eval,math.X1.PrettyP,math.X1.MultBy).contains(potentialRequest.op) &&
//          Set(math.X2.Times).contains(potentialRequest.tpeCase)
//      }
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        // TODO: dependency fix
        None
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import ffiStrings.stringCapabilities._
        import AnyParadigm.syntax._

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.X2.Times => mult(atts: _*)
                case _ => ???
              }

            case math.X1.MultBy =>
              onRequest.tpeCase match {
                case other@math.X2.Times =>
                  val lAtt = other.attributes.head
                  val rAtt = other.attributes.tail.head

                  for {
                    left <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(lAtt),
                      math.M0.getModel.baseDataType,
                      onRequest.request
                    ))
                    right <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(rAtt),
                      math.M0.getModel.baseDataType,
                      onRequest.request
                    ))

                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left, right)
                  } yield res
                case _ => ???
              }

            case math.X1.PrettyP =>
              onRequest.tpeCase match {
                case math.X2.Times => makeString(atts, "(", "*", ")")
                case _ => ???
              }
            case _ => ???
          }

        val result =
          for {
            atts <- forEach (onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M0.getModel.baseDataType,
                onRequest.request
              ))
            }
            res <- operate(atts)
          } yield res

        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(x2Provider, x1Provider)
  }
}
