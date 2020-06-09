package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}

object X4 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (x2x3Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val x4Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.X4.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      def applicable
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        Set(math.M0.Eval, math.X1.PrettyP, math.X1.MultBy).contains(onRequest.request.op) &&
          Set(math.M3.Neg).contains(onRequest.tpeCase)
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import ffiArithmetic.arithmeticCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import AnyParadigm.syntax._

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.X4.Neg =>
                  for {
                    minusOne <- reify(TypeRep.Double, -1)
                    res <- mult(minusOne, atts.head)
                  } yield res
                case _ => ???
              }

            case math.X1.PrettyP =>
              onRequest.tpeCase match {
                case math.X4.Neg =>
                  for {
                    minus <- reify(TypeRep.String, "-")
                    res <- stringAppend(minus, atts.head)
                  } yield res
                case _ => ???
              }

            case math.X1.MultBy =>
              onRequest.tpeCase match {
                case other@math.X4.Neg =>
                  val innerAtt = other.attributes.head

                  for {
                    inner <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(innerAtt),
                      math.M0.getModel.baseDataType,
                      onRequest.request,
                      Some(onRequest)
                    ))

                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, inner)
                  } yield res
              }

            case _ => ???
          }

        val result =
          for {
            atts <- forEach (onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M0.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
            }
            res <- operate(atts)
          } yield res
        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(x4Provider, x2x3Provider)
  }
}
