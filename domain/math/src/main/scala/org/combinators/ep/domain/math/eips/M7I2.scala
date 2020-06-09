package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{GenericModel, abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm

/** Upon merging M7 and I2 there is a need for MultByx(Divd, Mult, Neg) as well as a need for
 * (Collect,Simplify,Id,AsTree,Equals,PowBy)xPower
 *
 * These all have to be captured here...
 */
object M7I2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m7Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m7i2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M7I2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m7Provider.initialize(forApproach)
          _ <- i2Provider.initialize(forApproach)
        } yield ()
      }

      /** Nothing special here */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        Set.empty
      }

      def applicable
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (Set(math.M7.PowBy, math.M4.Simplify).contains(onRequest.request.op) &&
          Set(math.I2.Power).contains(onRequest.tpeCase)) ||
          (Set(math.I1.MultBy).contains(onRequest.request.op) &&
            Set(math.M3.Divd, math.M3.Mult, math.M3.Neg).contains(onRequest.tpeCase))
      }

      override def applicableIn(forApproach:  AIP[paradigm.type])(onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression],currentModel:GenericModel): Option[GenericModel] = {
        // must be designed to only return (to be safe) Java-accessible which is former branch only one step in past.
        val forwardTable:PartialFunction[(Operation,DataTypeCase),GenericModel] = {
          case (math.I1.MultBy, math.M3.Divd) => model // I HANDLE these
          case (math.I1.MultBy, math.M3.Mult) => model // I HANDLE these
          case (math.I1.MultBy, math.M3.Neg) => model  // I HANDLE these
          case (math.I1.MultBy, _) => math.I1.getModel

          case (math.M2.PrettyP, math.I2.Power) => math.I2.getModel
          case (math.M2.PrettyP, _) => math.M3.getModel

          case (math.M4.Collect, math.I2.Power) => model
          case (math.M4.Collect, _) => math.M4.getModel

          case (math.M4.Simplify, math.I2.Power) => model
          case (math.M4.Simplify, _) => math.M4.getModel

          case (math.M5.Identifier, math.I2.Power) => model
          case (math.M5.Identifier, _) => math.M5.getModel

          case (Operation.asTree, math.I2.Power) => model
          case (Operation.asTree, _) => math.M5.getModel

          case (math.M6.Equals, math.I2.Power) => model
          case (math.M6.Equals, _) => math.M6.getModel

          case (math.M7.PowBy, math.I2.Power) => model
          case (math.M7.PowBy, _) => math.M7.getModel
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

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._

        // useful. Might make it larger in scope
        def simplifyRec(att:abstractions.Attribute, attExpr: paradigm.syntax.Expression): Generator[MethodBodyContext, paradigm.syntax.Expression] = {
          forApproach.dispatch(
            SendRequest(
              attExpr,
              math.M4.getModel.baseDataType,
              Request(math.M4.Simplify, Map.empty),
              Some(onRequest)
            )
          )
        }

        val result = onRequest.request.op match {
          case mb@math.I1.MultBy =>
            // WE CAN OPTIMIZE MultBy with Mult

             for {
               res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
             } yield res

          // Collect already handled by original logic; same as AsTree; same as Equals; same as Identifier
          case s@math.M4.Simplify =>
            val atts = onRequest.attributes.keys.toSeq
            val attExprs = onRequest.attributes.values.toSeq
            for {
              lsimp <- simplifyRec(atts.head, attExprs.head)
              rsimp <- simplifyRec(atts.tail.head, attExprs.tail.head)
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.I2.Power, lsimp, rsimp)
            } yield res

          case p@math.M7.PowBy =>
          // must handle Power dataType. HERE WE CAN OPTIMIZED.
          val atts = onRequest.attributes.keys.toSeq
            val attExprs = onRequest.attributes.values.toSeq
            for {
              lsimp <- simplifyRec(atts.head, attExprs.head)
              rsimp <- simplifyRec(atts.tail.head, attExprs.tail.head)
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.I2.Power, lsimp, rsimp)
            } yield res
        }

        result.map(Some(_))
      }
    }

    // ORDER MATTERS! Need newest first, then subsequent branches shouldn't matter
    monoidInstance.combine(m7i2Provider, monoidInstance.combine(m7Provider, i2Provider))
  }
}