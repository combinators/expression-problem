package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.Strings

object A1M3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
  a1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type])
  :
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val a1m3_provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.A1M3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiStrings.enable()
          _ <- m3Provider.initialize(forApproach)
          _ <- a1Provider.initialize(forApproach)
        } yield ()
      }

      /** Nothing special here */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        Set.empty
      }

      override def applicableIn
        (forApproach:  AIP[paradigm.type], onRequest: PotentialRequest, currentModel:GenericModel): Option[GenericModel] = {

        val forwardTable:PartialFunction[(Operation,DataTypeCase),GenericModel] = {
          case (math.I1.MultBy, math.M3.Divd) => model // I HANDLE these
          case (math.I1.MultBy, math.M3.Mult) => model // I HANDLE these
          case (math.I1.MultBy, math.M3.Neg) => model  // I HANDLE these
          case (math.I1.MultBy, math.A1.Times) => math.A1.getModel

          case (math.I1.MultBy, _) => math.I1.getModel
          case (_, math.M3.Divd) => math.M3.getModel
          case (_, math.M3.Mult) => math.M3.getModel
          case (_, math.M3.Neg) => math.M3.getModel

            // handles everything else
          case _ => math.A1.getModel
        }

        val tblModel = forwardTable.lift(onRequest.op, onRequest.tpeCase)

        // Because EIP could be "further in future" then a given model, we need to be sure to
        // only return forwarding information when we have a hit on the currentModel.
        if (model == currentModel || model.before(currentModel)) {
          tblModel
        } else {
          None
        }
      }

      def applicable
      (forApproach: AIP[paradigm.type], onRequest: PotentialRequest): Boolean = {
        Set(math.I1.MultBy).contains(onRequest.op) &&
          Set(math.M3.Mult,math.M3.Divd,math.M3.Neg).contains(onRequest.tpeCase)
      }

      // NEED this since I have stated I will handle some of these
      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import AnyParadigm.syntax._

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case mb@math.I1.MultBy =>      // take advantage of Mult data type
              for {
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
              } yield res

            case _ => ???
          }

        val result =
          for {
            atts <- forEach (onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M3.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
            }
            res <- operate(atts)
          } yield res

        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(a1m3_provider, monoidInstance.combine(a1Provider, m3Provider))
  }
}
