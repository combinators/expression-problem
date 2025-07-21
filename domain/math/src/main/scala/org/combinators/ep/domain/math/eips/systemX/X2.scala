package org.combinators.ep.domain.math.eips.systemX     /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemX
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

import scala.language.postfixOps

object X2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (x1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val x2Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemX.X2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      // X2: "x2", Seq(Times), Seq.empty)
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (Set(math.M0.Eval, math.systemX.X1.PrettyP, math.systemX.X1.MultBy).contains(potentialRequest.op) && Set(systemX.X2.Times).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        def operate(attGenerators: Seq[Generator[paradigm.MethodBodyContext, syntax.Expression]]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case systemX.X2.Times =>
                  for {
                    atts <- forEach(attGenerators)(g => g)
                    result <- mult(atts*)
                  } yield result

                case _ => ???
              }

            case math.systemX.X1.MultBy =>
              onRequest.tpeCase match {
                case other@systemX.X2.Times =>
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

            case math.systemX.X1.PrettyP =>
              onRequest.tpeCase match {
                case systemX.X2.Times =>
                  for {
                    atts <- forEach(attGenerators)(g => g)
                    result <- makeString(atts, "(", "*", ")")
                  } yield result

                case _ => ???
              }
            case _ => ???
          }

        val attGenerators = onRequest.tpeCase.attributes.map { att =>
          forApproach.dispatch(SendRequest(
            onRequest.attributes(att),
            math.M0.getModel.baseDataType,
            onRequest.request
          ))
        }

        val result =
          for {
            res <- operate(attGenerators)
          } yield res

        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(x2Provider, x1Provider)
  }
}
