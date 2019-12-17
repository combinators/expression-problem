package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings, Trees}

object M5 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (m4Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int],
     ffiTrees: Trees.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val treeIdProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m4Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiTrees.enable()
        } yield ()
      }

      /** Simplify depends upon having a working eval. */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        if (op == Operation.asTree) {
          Set(math.M5.Identifier)
        } else {
          Set.empty
        }
      }

      def applicable
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (Set(math.M5.Identifier, Operation.asTree).contains(onRequest.request.op))
      }

      def logic
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
                        Request(Operation.asTree, Map.empty),
                        Some(onRequest)
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
                    Request(math.M5.Identifier, Map.empty),
                    Some(onRequest)
                  )
                )
              inst <- createNode(id, children)
            } yield Some(inst)

          case math.M5.Identifier =>
            reify(TypeRep.Int, onRequest.tpeCase.name.hashCode).map(Some(_))
          case _ => ???
        }
      }
    }
    monoidInstance.combine(treeIdProvider, m4Provider)
  }
}
