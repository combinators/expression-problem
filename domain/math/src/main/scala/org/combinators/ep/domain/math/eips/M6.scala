package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.Equality

object M6 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (m5Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val equalsProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M6.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m5Provider.initialize(forApproach)
          _ <- ffiEquality.enable()
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
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (Set(math.M6.Equals).contains(onRequest.request.op) &&
          // Constraint to ensure we have an implementation for asTree, which is used in this equality implementation provider
          m5Provider.applicable(forApproach)(onRequest.copy(request = Request(Operation.asTree, Map.empty))))
      }

      def logic
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import ffiEquality.equalityCapabilities._
        onRequest.request.op match {
          case math.M6.Equals =>
            for {
              selfTree <- forApproach.dispatch(
                  SendRequest(
                    onRequest.selfReference,
                    onRequest.onType,
                    Request(Operation.asTree, Map.empty),
                    Some(onRequest)
                  )
                )
              otherTree <- forApproach.dispatch(
                SendRequest(
                  onRequest.request.arguments.toSeq.head._2,
                  onRequest.onType,
                  Request(Operation.asTree, Map.empty),
                  Some(onRequest)
                )
              )
              treeTpe <- toTargetLanguageType(TypeRep.Tree)
              eq <- areEqual(treeTpe, selfTree, otherTree)
            } yield Some(eq)
          case _ => ???
        }
      }
    }
    // newest one must come first
    monoidInstance.combine(equalsProvider, m5Provider)
  }
}