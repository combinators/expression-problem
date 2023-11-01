package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}

// ToDo: MultBy should be simplified to MULT for all cases (not just Mult)...

object J2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j1Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.J2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j1Provider.initialize(forApproach)
          _ <- ffiEquality.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiArithmetic.enable()
        } yield ()
      }

//      /** Equals depends upon asTree method */
//      override def dependencies(op:Operation, dt:DataTypeCase) : Option[Set[Operation]] = {
//        op match {
//          case math.J2.Eql => Some(math.J2.isOps(model.flatten.typeCases).toSet)
//          case op if math.J2.isOps(model.flatten.typeCases).contains(op) => Some(Set(math.J2.Eql))
//          case _ => None
//        }
//      }
//
//      def applicable
//      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
//        (Set(math.J2.Mult).contains(potentialRequest.tpeCase) && model.flatten.ops.contains(potentialRequest.op)) ||
//          (model.flatten.typeCases.contains(potentialRequest.tpeCase) && model.ops.contains(potentialRequest.op))   // needed for isTypeCase x past Type Cases
//      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        // TODO: dependency fix
        None
      }

//      // need to say we are NOT applicable when doing MultBy while still in J1
//      override def applicableIn(forApproach:  AIP[paradigm.type], onRequest: PotentialRequest,currentModel:GenericModel): Option[GenericModel] = {
//        // must be designed to only return (to be safe) Java-accessible which is former branch only one step in past.
//
//        // NEEDED because of overwriting an existing MultBy with Mult.
//
//        // Because EIP could be "further in future" then a given model, we need to be sure to
//        // only return forwarding information when we have a hit on the currentModel.
//        if (model == currentModel) {
//          Some(model)
//        } else {
//          None  // no idea who can handle previous stuff
//        }
//      }

      /** Can handle any equals requests, by constructing Trees from Expressions. */
      override def genericLogic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import ffiEquality.equalityCapabilities._
        onRequest.request.op match {
          case math.J2.Eql =>
            //    default Boolean eql(Exp that) { return that.isMult(getLeft(), getRight()); }
            val op:Operation = math.J2.isOp(onRequest.tpeCase)
            for {
              other <- forApproach.dispatch(
                SendRequest(
                  onRequest.request.arguments.toSeq.head._2,   // 'other'
                  onRequest.onType,
                  Request(op, op.parameters.zip(onRequest.attributes.values).toMap)
                )
              )
            } yield Some(other)

          // default Boolean isAdd(Exp left, Exp right) {
          //        return left.eql(getLeft()) && right.eql(getRight());
          //    }
          case op if op == math.J2.isOp(onRequest.tpeCase) => {
            import ffiBoolean.booleanCapabilities._
            for {
              res <- forEach(onRequest.attributes.toSeq) { att => {
                val arg = onRequest.request.arguments.find(p => p._1.name == att._1.name).get
                if (att._1.tpe.isModelBase(model)) {
                  forApproach.dispatch(
                    SendRequest(
                      att._2,
                      onRequest.onType,
                      Request(math.J2.Eql, math.J2.Eql.parameters.zip(Seq(arg._2)).toMap)
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
          case op if op != math.J2.isOp(onRequest.tpeCase) && op.tags.contains(math.J2.IsOp) => {
            import ffiBoolean.booleanCapabilities._
            for {
              booleanFalse <- falseExp
            } yield Some(booleanFalse)
          }
          case _ => j1Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import ffiEquality.equalityCapabilities._
        import ffiArithmetic.arithmeticCapabilities._

        // assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name) TODO: fix assert
        onRequest.request.op match {
          case math.J2.Eql =>  genericLogic(forApproach)(onRequest)
          case op if op == math.J2.isOp(onRequest.tpeCase) => genericLogic(forApproach)(onRequest)
          case op if op != math.J2.isOp(onRequest.tpeCase) && op.tags.contains(math.J2.IsOp)  => genericLogic(forApproach)(onRequest)

          /** Need to dispatch 'eval' to the left and right. */
          case op if op == math.M0.Eval && onRequest.tpeCase == math.J2.Mult =>
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(onRequest.tpeCase.attributes.head),  // instead use look-up addC.attributes.find(att => att.name)
                math.M0.getModel.baseDataType,
                onRequest.request
              ))
              right <- forApproach.dispatch(SendRequest(
                onRequest.attributes(onRequest.tpeCase.attributes.tail.head),
                math.M0.getModel.baseDataType,
                onRequest.request
              ))

              // FFI capability provided in truly language independent manner
              res <- mult(left, right)
            } yield Some(res)

         case mb@math.J1.MultBy =>    // WE CAN OPTIMIZE MultBy with Mult
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.J2.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case _ => ???
        }
      }
    }
    // newest one must come first
    monoidInstance.combine(j2Provider, j1Provider)
  }
}
