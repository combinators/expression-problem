package org.combinators.ep.domain.math.eips.systemO   /*DD:LI:AI*/

import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object OD1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (m2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
       ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val od1Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemO.OD1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- m2Provider.initialize(forApproach)
        } yield ()
      }
      
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val ops = math.systemO.OD1.getModel.flatten.ops
        if (Seq(math.systemO.OD1.Mult).contains(potentialRequest.tpeCase) && ops.contains(potentialRequest.op)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        m2Provider.genericLogic(forApproach)(onRequest)

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

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.systemO.OD1.Mult => mult(atts*)
                case _ => ???
              }

            case math.M2.PrettyP =>
              onRequest.tpeCase match {
                case math.systemO.OD1.Mult => makeString(atts, "(", "*", ")")
                case _ => ???
              }
            case _ => ???
          }

        val result =
            for {
              atts <- forEach (onRequest.tpeCase.attributes) { att =>
                  forApproach.dispatch(SendRequest(
                    onRequest.attributes(att),
                    math.systemO.OD1.getModel.baseDataType,
                    onRequest.request
                  ))
                }
              res <- operate(atts)
            } yield res
        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(od1Provider, m2Provider)
  }
}
