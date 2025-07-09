package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.ep.domain.abstractions.{DataTypeCase, DomainTpeRep, Operation}
import org.combinators.ep.domain.extensions._
import org.combinators.ep.domain.math
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import AnyParadigm.syntax.forEach
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.cogen.paradigm.ffi.{Booleans, Equality}

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

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.M6.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          potentialRequest.op match {
            case math.M6.Equals => Some(Set(Operation.asTree))
            case math.M6.Eql => Some(math.M6.isOps(Seq(potentialRequest.tpeCase)).toSet)
            case isOp if math.M6.isOps(cases).contains(isOp) => Some(if (isOp == math.M6.isOp(potentialRequest.tpeCase)) Set(math.M6.Eql) else Set.empty)
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
        import paradigm._
        import methodBodyCapabilities._
        import ffiEquality.equalityCapabilities._
        onRequest.request.op match {
          case math.M6.Eql =>
            //    default Boolean eql(Exp that) { return that.isMult(getLeft(), getRight()); }
            val op:Operation = math.M6.isOp(onRequest.tpeCase)
            for {
              other <- forApproach.dispatch(
                SendRequest(
                  onRequest.request.arguments.toSeq.head._2,   // 'other'
                  onRequest.onType,
                  Request(op, op.parameters.zip(onRequest.attributes.values).toMap)
                )
              )
            } yield Some(other)

          case math.M6.Equals =>
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
              treeTpe <- toTargetLanguageType(DomainTpeRep.Tree)
              eq <- areEqual(treeTpe, selfTree, otherTree)
            } yield Some(eq)

          // default Boolean isAdd(Exp left, Exp right) {
          //        return left.eql(getLeft()) && right.eql(getRight());
          //    }
          case op if op == math.M6.isOp(onRequest.tpeCase) => {
            import ffiBooleans.booleanCapabilities._
            for {
              res <- forEach(onRequest.attributes.toSeq) { att => {
               val arg = onRequest.request.arguments.find(p => p._1.name == att._1.name).get
                if (att._1.tpe.isModelBase(model)) {
                  forApproach.dispatch(
                    SendRequest(
                      att._2,
                      onRequest.onType,
                      Request(math.M6.Eql, math.M6.Eql.parameters.zip(Seq(arg._2)).toMap)
                    )
                  )
                } else {
                  for {
                    myType <- toTargetLanguageType(arg._1.tpe)
                    eqleql <- areEqual(myType, att._2, arg._2)
                  } yield eqleql
                }
             }}

             // now construct X && y && z
              conjunction <- if (res.length == 1) { Command.lift[MethodBodyContext,paradigm.syntax.Expression](res.head) } else { and(res) }
            } yield Some(conjunction)
          }

            // need to know when isOp is not in the onRequest Type (to handle return false; default implementation)
            // because then we can return FALSE
          case op if op != math.M6.isOp(onRequest.tpeCase) && op.tags.contains(math.M6.IsOp) => {
            import ffiBooleans.booleanCapabilities._
            for {
              booleanFalse <- falseExp
            } yield Some(booleanFalse)
          }
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
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)
        onRequest.request.op match {
          case math.M6.Equals => genericLogic(forApproach)(onRequest)
          case math.M6.Eql =>  genericLogic(forApproach)(onRequest)
          case op if op == math.M6.isOp(onRequest.tpeCase) => genericLogic(forApproach)(onRequest)
          case op if op != math.M6.isOp(onRequest.tpeCase) && op.tags.contains(math.M6.IsOp)  => genericLogic(forApproach)(onRequest)

          case _ => ???
        }
      }
    }
    // newest one must come first
    monoidInstance.combine(equalsProvider, m5Provider)
  }
}