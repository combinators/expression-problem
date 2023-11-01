package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ffi.{Booleans, Equality}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}

object J5 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (j4Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiBooleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
    ):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j5Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.J4.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j4Provider.initialize(forApproach)
          _ <- ffiEquality.enable()
          _ <- ffiBooleans.enable()
        } yield ()
      }

//      /** Equals depends upon asTree method */
//      override def dependencies(op:Operation, dt:DataTypeCase) : Option[Set[Operation]] = {
//        op match {
//          case math.J5.Equals => Some(Set(Operation.asTree))
//          case _ => None
//        }
//      }
//
//      def applicable
//        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
//           Seq(math.J5.Equals).contains(potentialRequest.op) &&
//          // Constraint to ensure we have an implementation for asTree, which is used in this equality implementation provider
//          j4Provider.applicable(forApproach,potentialRequest.copy(op = Operation.asTree))
//      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        // TODO: dependency fix
        None
      }

      /** Can handle any equals requests, by constructing Trees from Expressions. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiEquality.equalityCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        onRequest.request.op match {

          case math.J5.Equals =>
            for {
              selfTree <- forApproach.dispatch(
                SendRequest(
                  onRequest.selfReference,
                  onRequest.onType,
                  Request(Operation.asTree, Map.empty)
                )
              )
              otherTree <- forApproach.dispatch(
                SendRequest(
                  onRequest.request.arguments.toSeq.head._2,
                  onRequest.onType,
                  Request(Operation.asTree, Map.empty)
                )
              )
              treeTpe <- toTargetLanguageType(TypeRep.Tree)
              eq <- areEqual(treeTpe, selfTree, otherTree)
            } yield Some(eq)

          case _ => j4Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        // assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name) TODO: fix assert

        onRequest.request.op match {
          case math.J5.Equals =>
            genericLogic(forApproach)(onRequest)

          case _ => ???
        }
      }
    }
    // newest one must come first
    monoidInstance.combine(j5Provider, j4Provider)
  }
}