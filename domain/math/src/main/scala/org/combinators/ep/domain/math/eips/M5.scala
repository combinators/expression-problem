package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Trees}

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

      /** AsTree depends upon Identifier. */
      override def dependencies(op:Operation, dt:DataTypeCase) : Option[Set[Operation]] = {
        if (op == Operation.asTree) {
          Some(Set(math.M5.Identifier))
        } else {
          None
        }
      }

      def applicable
        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        Set(math.M5.Identifier, Operation.asTree).contains(potentialRequest.op) &&
          Set(math.M3.Divd,math.M3.Mult, math.M3.Neg,math.M1.Sub,math.M0.Add,math.M0.Lit).contains(potentialRequest.tpeCase)
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
                  case (att@Attribute(_, TypeRep.DataType(dt)), attExp) =>
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
        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)
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
