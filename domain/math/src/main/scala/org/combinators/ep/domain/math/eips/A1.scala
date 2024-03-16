package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}

object A1  {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val a1Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.A1.getModel
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- i1Provider.initialize(forApproach)
        } yield ()
      }

      // A1 adds TIMES data type

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (Set(math.M0.Eval, math.M2.PrettyP, math.systemI.I1.MultBy).contains(potentialRequest.op) && Set(math.A1.Times).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
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

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)
        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.A1.Times => mult(atts: _*)
                case _ => ???
              }

//            case math.systemI.I1.MultBy =>
//              onRequest.tpeCase match {
//                case other@math.A1.Times =>
//                  val lAtt = other.attributes.head
//                  val rAtt = other.attributes.tail.head
//
//                  for {
//                    left <- forApproach.dispatch(SendRequest(
//                      onRequest.attributes(lAtt),
//                      math.M2.getModel.baseDataType,
//                      onRequest.request
//                    ))
//                    right <- forApproach.dispatch(SendRequest(
//                      onRequest.attributes(rAtt),
//                      math.M2.getModel.baseDataType,
//                      onRequest.request
//                    ))
//
//                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left, right)
//                  } yield res
//                case _ => ???
//              }

            case mb@math.systemI.I1.MultBy =>    // WE CAN OPTIMIZE MultBy with Mult
              for {
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.A1.Times, onRequest.selfReference, onRequest.request.arguments.head._2)
              } yield  res

            case math.M2.PrettyP =>
              onRequest.tpeCase match {
                case math.A1.Times => makeString(atts, "(", "x", ")")
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
    monoidInstance.combine(a1Provider, i1Provider)
  }
}
