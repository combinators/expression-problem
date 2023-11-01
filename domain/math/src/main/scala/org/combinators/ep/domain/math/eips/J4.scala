package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Trees}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J4 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (j3Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int],
     ffiTrees: Trees.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j4Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.J4.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j3Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiTrees.enable()
        } yield ()
      }

//      /** AsTree depends upon Identifier. */
//      override def dependencies(op:Operation, dt:DataTypeCase) : Option[Set[Operation]] = {
//        if (op == Operation.asTree) {
//          Some(Set(math.J4.Identifier))
//        } else {
//          None
//        }
//      }
//
//      def applicable
//        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
//        Set(math.J4.Identifier, Operation.asTree).contains(potentialRequest.op) &&
//          Set(math.M0.Add,math.M0.Lit,math.J1.Sub,math.J2.Mult, math.J3.Neg, math.J3.Divd).contains(potentialRequest.tpeCase)
//      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        // TODO: dependency fix
        None
      }

      /** Can handle any AsTree or Identifier operations. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiTrees.treeCapabilities._
        import paradigm._
        import methodBodyCapabilities._

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

          case _ => j3Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        // assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name) TODO: fix assert
        onRequest.request.op match {
          case math.J4.Identifier => genericLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => genericLogic(forApproach)(onRequest)
          case _ => ???
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(j4Provider, j3Provider)
  }
}
