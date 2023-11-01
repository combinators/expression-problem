package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object D3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (d1d2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val d3Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.D3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (Set(math.D3.PrettyP).contains(potentialRequest.op) && Set(math.M0.Lit, math.M0.Add, math.M1.Sub, math.D2.Mult).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }
      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        d1d2Provider.genericLogic(forApproach)(onRequest)

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        // assert(applicable(forApproach)(onRequest)) TODO: fix assert

        val result = onRequest.tpeCase match {
          case litC@math.M0.Lit =>
            val att = litC.attributes.head
            for {
              ty <- toTargetLanguageType(att.tpe)
              res <- asString(onRequest.attributes(att), ty)
            } yield res

          case other =>
            val lAtt = other.attributes.head
            val rAtt = other.attributes.tail.head
            val operator =
              other match {
                case math.M0.Add => "+"
                case math.M1.Sub => "-"
                case math.D2.Mult => "*"
                case _ => ???
              }
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
              res <- makeString(Seq(left, right), "(", operator, ")")
            } yield res
        }
        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(d3Provider, d1d2Provider)
  }
}