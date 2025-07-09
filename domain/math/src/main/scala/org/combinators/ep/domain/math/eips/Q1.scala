package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Arithmetic, RealArithmetic, Strings, Trees}
import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, DomainTpeRep}
import org.combinators.ep.domain.math
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
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
      override val model = math.Q1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m3w1Provider.initialize(forApproach)
          _ <- ffiRealArithmetic.enable()
          _ <- ffiIntArithmetic.enable()
          _ <- ffiTrees.enable()
        } yield ()
      }

      /** AsTree depends upon Identifier. */
//      override def dependencies(op:Operation, dt:DataTypeCase) : Option[Set[Operation]] = {
//        if (op == Operation.asTree) {
//          Some(Set(math.Q1.Identifier))
//        } else {
//          None
//        }
//      }
//
//      def applicable
//        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
//        (Set(math.Q1.Identifier, Operation.asTree).contains(potentialRequest.op) &&
//          Set(math.M3.Divd,math.M3.Mult, math.M3.Neg,math.M1.Sub,math.M0.Add,math.M0.Lit,math.W1.Power).contains(potentialRequest.tpeCase)) ||
//          (Set(math.Q1.Sqrt).contains(potentialRequest.tpeCase) && Set(math.M2.PrettyP,math.M0.Eval,math.Q1.Identifier,Operation.asTree).contains(potentialRequest.op))
//      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.Q1.getModel.flatten.typeCases
        (potentialRequest.op, potentialRequest.tpeCase) match {
          case (math.Q1.Identifier, tpeCase) if cases.contains(tpeCase) => Some(Set.empty)
          case (Operation.asTree, tpeCase) if cases.contains(tpeCase) => Some(Set(math.Q1.Identifier))
          case (math.M2.PrettyP, math.Q1.Sqrt) => Some(Set.empty)
          case (math.M0.Eval, math.Q1.Sqrt) => Some(Set.empty)

          case (_, _) => None
        }
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
                    Request(math.Q1.Identifier, Map.empty)
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

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {
          case math.Q1.Identifier => genericLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => genericLogic(forApproach)(onRequest)
          case math.M0.Eval =>      // restricted in applicable above to only apply to sqrt
              for {
                inner <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(onRequest.tpeCase.attributes.head),
                  math.M1.getModel.baseDataType,
                  onRequest.request
                ))

                res <- sqrt(inner)
              } yield Some(res)

          case math.M2.PrettyP =>   // restricted in applicable above to only apply to sqrt
            for {
              inner <- forApproach.dispatch(SendRequest(
                onRequest.attributes(onRequest.tpeCase.attributes.head),
                math.M1.getModel.baseDataType,
                onRequest.request
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
