package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{abstractions, math}
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
    val combinedProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
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
          Set(math.I1.MultBy).contains(onRequest.request.op) // all MultBy optimized with Mult now
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

    // ORDER MATTERS! First use logic from each of the two branches, and then (at the end)
    // resolve here in combined
    monoidInstance.combine(i2Provider, monoidInstance.combine(m7Provider, combinedProvider))
  }
}