package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm

object X2X3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (x2Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   x3Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  :
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val x2x3_provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.X2X3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- x2Provider.initialize(forApproach)
          _ <- x3Provider.initialize(forApproach)
        } yield ()
      }

      /** Nothing special here */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        Set.empty
      }

      override def applicableIn
      (forApproach:  AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression],currentModel:GenericModel): Option[GenericModel] = {

        val forwardTable:PartialFunction[(Operation,DataTypeCase),GenericModel] = {
          case (_, math.X2.Times) => math.X2.getModel
          case (_, math.X3.Divd) => math.X3.getModel

          case (math.X1.MultBy, math.X1.Sub) => math.X3.getModel   // COULD BE EITHER ONE!!!
          case (math.X1.MultBy, math.M0.Add) => math.X3.getModel   // COULD BE EITHER ONE!!!
          case (math.X1.MultBy, math.M0.Lit) => math.X3.getModel   // COULD BE EITHER ONE!!!

          case (math.X1.PrettyP, math.X1.Sub) => math.X3.getModel   // COULD BE EITHER ONE!!!
          case (math.X1.PrettyP, math.M0.Add) => math.X3.getModel   // COULD BE EITHER ONE!!!
          case (math.X1.PrettyP, math.M0.Lit) => math.X3.getModel   // COULD BE EITHER ONE!!!

          case (math.M0.Eval, math.X1.Sub) => math.X3.getModel      // COULD BE EITHER ONE!!!
          case (math.M0.Eval, math.M0.Add) => math.X3.getModel      // COULD BE EITHER ONE!!!
          case (math.M0.Eval, math.M0.Lit) => math.X3.getModel      // COULD BE EITHER ONE!!!

          // handles everything else
          case _ => math.X2X3.getModel
        }

        val tblModel = forwardTable.lift(onRequest.request.op, onRequest.tpeCase)

        // Because EIP could be "further in future" then a given model, we need to be sure to
        // only return forwarding information when we have a hit on the currentModel.
        if (model == currentModel || model.before(currentModel)) {
          tblModel
        } else {
          None
        }
      }

      def applicable
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        false
      }

      // HACK: THIS CAN ENTIRELY BE REMOVED BUT HOW TO DO SKIP HERE?
      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import AnyParadigm.syntax._

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {

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

    // newest first
    monoidInstance.combine(x2x3_provider, monoidInstance.combine(x3Provider, x2Provider))
  }
}
