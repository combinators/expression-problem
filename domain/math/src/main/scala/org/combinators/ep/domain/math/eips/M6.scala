package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ffi.{Booleans, Equality}

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

      /** Equals depends upon asTree method */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        op match {
          case math.M6.Equals => Set(Operation.asTree)
          case _ => Set.empty
        }
      }

      def applicable
        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        potentialRequest.op.tags.contains(math.M6.IsOp) ||
          (math.M6.getModel.ops.contains(potentialRequest.op) &&
          // Constraint to ensure we have an implementation for asTree, which is used in this equality implementation provider
          m5Provider.applicable(forApproach,potentialRequest.copy(op = Operation.asTree)))
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
                  Request(op, op.parameters.zip(onRequest.attributes.values).toMap),
                  Some(onRequest)
                )
              )
            } yield Some(other)

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
                      Request(math.M6.Eql, math.M6.Eql.parameters.zip(Seq(arg._2)).toMap),
                      Some(onRequest)
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
        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)
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