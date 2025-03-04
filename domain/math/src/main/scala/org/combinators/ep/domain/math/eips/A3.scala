package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}

object A3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (a1m3i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val a3Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.A3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- a1m3i2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

    override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
      if ((potentialRequest.tpeCase == math.A3.Inv) && Set(math.M0.Eval, math.M2.PrettyP, math.systemI.I1.MultBy).contains(potentialRequest.op)) {
        Some(Set.empty)
      } else {
        None
      }
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

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.A3.Inv => div(Seq(atts.tail.head, atts.head): _*)   // FLIP
                case _ => ???
              }

            case math.systemI.I1.MultBy =>
              onRequest.tpeCase match {
                case other@math.A3.Inv =>
                  val lAtt = other.attributes.head
                  val rAtt = other.attributes.tail.head

                  for {
                    left <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(lAtt),
                      math.M2.getModel.baseDataType,
                      onRequest.request
                    ))
                    right <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(rAtt),
                      math.M2.getModel.baseDataType,
                      onRequest.request
                    ))

                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left, right)
                  } yield res

                case _ => ???
              }

            case math.M2.PrettyP =>
              onRequest.tpeCase match {
                case math.A3.Inv => makeString(Seq(atts.tail.head, atts.head), "(", "/", ")")
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
    // newest first
    monoidInstance.combine(a3Provider, a1m3i2Provider)
  }
}
