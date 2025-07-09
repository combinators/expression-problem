package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Trees}
import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, DomainTpeRep}
import org.combinators.ep.domain.math
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}

object M5 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (m4Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int],
     ffiTrees: Trees.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val treeIdProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M5.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m4Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiTrees.enable()
        } yield ()
      }
      

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.M5.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          potentialRequest.op match {
            case Operation.asTree => Some(Set(math.M5.Identifier))
            case math.M5.Identifier => Some(Set.empty)
            case _ => None
          }
        } else {
          None
        }
      }
      

      /** Can handle any AsTree or Identifier operations. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import AnyParadigm.syntax._
        import ffiTrees.treeCapabilities._

        onRequest.request.op match {
          case op if op == Operation.asTree =>
            for {
              children <- forEach (onRequest.attributes.toSeq) {
                  case (att@Attribute(_, DomainTpeRep.DataType(dt)), attExp) =>
                    forApproach.dispatch(
                      SendRequest(
                        attExp,
                        dt,
                        Request(Operation.asTree, Map.empty)
                      )
                    )
                  case (att@Attribute(_, attTpe), attExp) =>
                    for {
                      tpe <- toTargetLanguageType(attTpe)
                      result <- createLeaf(tpe, attExp)
                    } yield result
                }
              id <- forApproach.dispatch(
                  SendRequest(
                    onRequest.selfReference,
                    onRequest.onType,
                    Request(math.M5.Identifier, Map.empty)
                  )
                )
              inst <- createNode(id, children)
            } yield Some(inst)

          case math.M5.Identifier =>
            reify(TypeRep.Int, onRequest.tpeCase.name.hashCode).map(Some(_))

          case _ => m4Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)
        onRequest.request.op match {
          case math.M5.Identifier => genericLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => genericLogic(forApproach)(onRequest)
          case _ => ???
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(treeIdProvider, m4Provider)
  }
}
