package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic, Strings, Trees}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object Q1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (m3w1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiIntArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int],
     ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiTrees: Trees.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val q1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m3w1Provider.initialize(forApproach)
          _ <- ffiRealArithmetic.enable()
          _ <- ffiIntArithmetic.enable()
          _ <- ffiTrees.enable()
        } yield ()
      }

      /** AsTree depends upon Identifier. */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        if (op == Operation.asTree) {
          Set(math.Q1.Identifier)
        } else {
          Set.empty
        }
      }

      def applicable
        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        (Set(math.Q1.Identifier, Operation.asTree).contains(potentialRequest.op) &&
          Set(math.M3.Divd,math.M3.Mult, math.M3.Neg,math.M1.Sub,math.M0.Add,math.M0.Lit,math.W1.Power).contains(potentialRequest.tpeCase)) ||
          (Set(math.Q1.Sqrt).contains(potentialRequest.tpeCase) && Set(math.M2.PrettyP,math.M0.Eval,math.Q1.Identifier,Operation.asTree).contains(potentialRequest.op))
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
                    Request(math.Q1.Identifier, Map.empty),
                    Some(onRequest)
                  )
                )
              inst <- createNode(id, children)
            } yield Some(inst)

          case math.Q1.Identifier =>
            reify(TypeRep.Int, onRequest.tpeCase.name.hashCode).map(Some(_))

          case _ => m3w1Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import ffiRealArithmetic.realArithmeticCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)
        onRequest.request.op match {
          case math.Q1.Identifier => genericLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => genericLogic(forApproach)(onRequest)
          case math.M0.Eval =>      // restricted in applicable above to only apply to sqrt
              for {
                inner <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(onRequest.tpeCase.attributes.head),
                  math.M1.getModel.baseDataType,
                  onRequest.request,
                  Some(onRequest)
                ))

                res <- sqrt(inner)
              } yield Some(res)

          case math.M2.PrettyP =>   // restricted in applicable above to only apply to sqrt
            for {
              inner <- forApproach.dispatch(SendRequest(
                onRequest.attributes(onRequest.tpeCase.attributes.head),
                math.M1.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))

              front <- reify(TypeRep.String, "(sqrt ")
              back <-  reify(TypeRep.String, ")")
              res <- stringAppend(front, inner, back)
            } yield Some(res)

          case _ => ???
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(q1Provider, m3w1Provider)
  }
}
