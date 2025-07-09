package org.combinators.ep.domain.math.eips.systemJ   /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.ep.domain.extensions._
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemJ
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import AnyParadigm.syntax.forEach
import org.combinators.cogen.Command
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans, Equality}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j2Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemJ.J2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j1Provider.initialize(forApproach)
          _ <- ffiEquality.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiArithmetic.enable()
        } yield ()
      }

      //  Seq(Mult), Seq(Eql)  ++ isOps(allTypes))

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.systemJ.J2.getModel.flatten.typeCases
        val ops = math.systemJ.J2.getModel.flatten.ops
        (potentialRequest.op, potentialRequest.tpeCase) match {
          case (math.systemJ.J2.Eql, _) => Some(Set(math.systemJ.J2.isOp(potentialRequest.tpeCase)))

          // isMult => empty for any non MULT argument (returns false), eql for mult argument (left and right eql)
          case (isOp, tpeCase) if math.systemJ.J2.isOps(cases).contains(isOp) => Some(if (isOp == math.systemJ.J2.isOp(tpeCase)) Set(math.systemJ.J2.Eql) else Set.empty)
          // isXXX for M argument => empty, e.g. isMult(ADD) = false
          //case (isOp, math.systemJ.J2.Mult) if math.systemJ.J2.isOps(cases).contains(isOp) => Some(Set.empty)

          // here is where Eval and MultBy are processed. MUST COME AFTER EARLIER CHECK FOR ISOP...
          case (_, math.systemJ.J2.Mult) if ops.contains(potentialRequest.op) => Some(Set.empty)
          case (math.systemJ.J1.MultBy, _) if cases.contains(potentialRequest.tpeCase) => Some(Set.empty)

          // rest handled above by first two cases
          case (_, _) => None
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
          case math.systemJ.J2.Eql =>
            //    default Boolean eql(Exp that) { return that.isMult(getLeft(), getRight()); }
            val op: Operation = math.systemJ.J2.isOp(onRequest.tpeCase)
            for {
              other <- forApproach.dispatch(
                SendRequest(
                  onRequest.request.arguments.toSeq.head._2, // 'other'
                  onRequest.onType,
                  Request(op, op.parameters.zip(onRequest.attributes.values).toMap)
                )
              )
            } yield Some(other)

          // default Boolean isAdd(Exp left, Exp right) {
          //        return left.eql(getLeft()) && right.eql(getRight());
          //    }
          case op if op == math.systemJ.J2.isOp(onRequest.tpeCase) => {
            import ffiBoolean.booleanCapabilities._
            for {
              res <- forEach(onRequest.attributes.toSeq) { att => {
                val arg = onRequest.request.arguments.find(p => p._1.name == att._1.name).get
                if (att._1.tpe.isModelBase(model)) {
                  forApproach.dispatch(
                    SendRequest(
                      att._2,
                      onRequest.onType,
                      Request(math.systemJ.J2.Eql, math.systemJ.J2.Eql.parameters.zip(Seq(arg._2)).toMap)
                    )
                  )
                } else {
                  for {
                    myType <- toTargetLanguageType(arg._1.tpe)
                    eqleql <- areEqual(myType, att._2, arg._2)
                  } yield eqleql
                }
              }
              }

              // now construct X && y && z
              conjunction <- if (res.length == 1) {
                Command.lift[MethodBodyContext, paradigm.syntax.Expression](res.head)
              } else {
                and(res)
              }
            } yield Some(conjunction)
          }

          // need to know when isOp is not in the onRequest Type (to handle return false; default implementation)
          // because then we can return FALSE
          case op if op != math.systemJ.J2.isOp(onRequest.tpeCase) && op.tags.contains(math.systemJ.J2.IsOp) => {
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
        import ffiArithmetic.arithmeticCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {
          case math.systemJ.J2.Eql => genericLogic(forApproach)(onRequest)
          case op if op == math.systemJ.J2.isOp(onRequest.tpeCase) => genericLogic(forApproach)(onRequest)
          case op if op != math.systemJ.J2.isOp(onRequest.tpeCase) && op.tags.contains(math.systemJ.J2.IsOp) => genericLogic(forApproach)(onRequest)

          /** Need to dispatch 'eval' to the left and right. */
          case op if op == math.M0.Eval && onRequest.tpeCase == math.systemJ.J2.Mult =>
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(onRequest.tpeCase.attributes.head), // instead use look-up addC.attributes.find(att => att.name)
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

          case mb@math.systemJ.J1.MultBy => // WE CAN OPTIMIZE MultBy with Mult
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.systemJ.J2.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case _ => ???
        }
      }
    }
    // newest one must come first
    monoidInstance.combine(j2Provider, j1Provider)
  }
}
