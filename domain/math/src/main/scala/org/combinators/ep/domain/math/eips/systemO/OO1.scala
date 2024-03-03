package org.combinators.ep.domain.math.eips.systemO      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Operation, TypeRep}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic, Strings}

object OO1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val oo1Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemO.OO1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.systemO.OO1.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          (potentialRequest.op, potentialRequest.tpeCase) match {
            case (math.systemO.OO1.Atomic, _) => Some(Set.empty)
            case (_, _) => None
          }
        } else {
          None
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        val result = onRequest.tpeCase match {
          case litC@math.M0.Lit =>
            for {
              res <- makeString(Seq.empty, "L", "", "")
            } yield res

          case other =>
            val lAtt = other.attributes.head
            val rAtt = other.attributes.tail.head
            val operator =
              other match {
                case math.M0.Add => "+"
                case math.M1.Sub => "-"
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

    // newest first
    monoidInstance.combine(oo1Provider, m2Provider)
  }
}
