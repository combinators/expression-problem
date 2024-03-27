package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}

object M3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (m2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
       ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.M3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- m2Provider.initialize(forApproach)
        } yield ()
      }
      
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val ops = math.M3.getModel.flatten.ops
        if ((math.M3.getModel.typeCases.contains(potentialRequest.tpeCase)) && ops.contains(potentialRequest.op)) {
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
                case math.M3.Divd => div(atts: _*)
                case math.M3.Mult => mult(atts: _*)
                case math.M3.Neg =>
                  for {
                    minusOne <- reify(TypeRep.Double, -1)
                    res <- mult(minusOne, atts.head)
                  } yield res
                case _ => ???
              }

            case math.M2.PrettyP =>
              onRequest.tpeCase match {
                case math.M3.Divd => makeString(atts, "(", "/", ")")
                case math.M3.Mult => makeString(atts, "(", "*", ")")
                case math.M3.Neg =>
                  for {
                    minus <- reify(TypeRep.String, "-")
                    res <- stringAppend(minus, atts.head)
                  } yield res
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
    monoidInstance.combine(m3Provider, m2Provider)
  }
}
