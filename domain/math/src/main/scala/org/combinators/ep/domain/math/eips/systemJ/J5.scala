package org.combinators.ep.domain.math.eips.systemJ   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Operation, TypeRep}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemJ
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Booleans, Equality}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J5 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j4Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiBooleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j5Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemJ.J5.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j4Provider.initialize(forApproach)
          _ <- ffiEquality.enable()
          _ <- ffiBooleans.enable()
        } yield ()
      }

      // J5 adds  Seq.empty, Seq(Equals))
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.systemJ.J5.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          potentialRequest.op match {
            case math.systemJ.J5.Equals => Some(Set(Operation.asTree))
            case _ => None
          }
        } else {
          None
        }
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

          case math.systemJ.J5.Equals =>
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

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {
          case math.systemJ.J5.Equals =>
            genericLogic(forApproach)(onRequest)

          case _ => ???
        }
      }
    }
    // newest one must come first
    monoidInstance.combine(j5Provider, j4Provider)
  }
}
