package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Booleans, Equality}

object M6 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (m5Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiBooleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
    ):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val equalsProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M6.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m5Provider.initialize(forApproach)
          _ <- ffiEquality.enable()
          _ <- ffiBooleans.enable()
        } yield ()
      }

      /** Equals depends upon asTree method */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        op match {
          case math.M6.Equals => Set(Operation.asTree)
          case _ => Set.empty
        }
      }

      def applicable
        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        Set(math.M6.Equals).contains(potentialRequest.op) &&
          // Constraint to ensure we have an implementation for asTree, which is used in this equality implementation provider
          m5Provider.applicable(forApproach,potentialRequest.copy(op = Operation.asTree))
      }

      /** Can handle any equals requests, by constructing Trees from Expressions. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import ffiEquality.equalityCapabilities._
        onRequest.request.op match {
          case math.M6.Equals =>
//            for {
//              selfTree <- forApproach.dispatch(
//                SendRequest(
//                  onRequest.selfReference,
//                  onRequest.onType,
//                  Request(Operation.asTree, Map.empty),
//                  Some(onRequest)
//                )
//              )
//              otherTree <- forApproach.dispatch(
//                SendRequest(
//                  onRequest.request.arguments.toSeq.head._2,
//                  onRequest.onType,
//                  Request(Operation.asTree, Map.empty),
//                  Some(onRequest)
//                )
//              )
//              treeTpe <- toTargetLanguageType(TypeRep.Tree)
//              eq <- areEqual(treeTpe, selfTree, otherTree)
//            } yield Some(eq)
            for {
              eq <- ffiBooleans.booleanCapabilities.trueExp
            } yield Some(eq)
          case _ => m5Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import ffiEquality.equalityCapabilities._
        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)
        onRequest.request.op match {
          case math.M6.Equals => genericLogic(forApproach)(onRequest)
          case _ => ???
        }
      }
    }
    // newest one must come first
    monoidInstance.combine(equalsProvider, m5Provider)
  }
}