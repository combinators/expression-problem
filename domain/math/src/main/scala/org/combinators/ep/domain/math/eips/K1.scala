package org.combinators.ep.domain.math.eips   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, RealArithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object K1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val k1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.K1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        // cannot forget that isXXX operations are dependencies (Extensible Visitor identified this oversight)
        val initial = math.J2.isOps(Seq(math.K1.Power)).toSet

        val new_ones = op match {
          case math.J2.Eql => math.J2.isOps(model.flatten.typeCases).toSet
          case op if math.J2.isOps(Seq(dt)).contains(op) => Set(math.J2.Eql)
          case op if Seq(math.J1.MultBy).contains(op) => Set(math.M0.Eval)    // needed for extensible visitor for K1 for some reason
          case _ => Set.empty
        }

        initial ++ new_ones
      }

      def applicable
      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        (Set(math.J1.MultBy,math.M0.Eval,math.J2.Eql).contains(potentialRequest.op) && Set(math.K1.Power).contains(potentialRequest.tpeCase)) ||
          math.J2.isOps(Seq(math.K1.Power)).contains(potentialRequest.op) || // needed for isTypeCase for new type cases being applied to old types... [handles isNeg x sub]
          Set(math.K1.Power).contains(potentialRequest.tpeCase)   // needed for isSub x Neg (and others)
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        j2Provider.genericLogic(forApproach)(onRequest)

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {

        import ffiArithmetic.arithmeticCapabilities._
        import ffiRealArithmetic.realArithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        assert(applicable(forApproach)(onRequest))

        // need to know when isOp is not in the onRequest Type (to handle return false; default implementation)
        // because then we can return FALSE
        if (onRequest.request.op != math.J2.isOp(onRequest.tpeCase) && onRequest.request.op.tags.contains(math.J2.IsOp)) {
          import ffiBoolean.booleanCapabilities._
          for {
            booleanFalse <- falseExp
          } yield Some(booleanFalse)
        } else if (onRequest.request.op == math.J2.isOp(onRequest.tpeCase)) {
          genericLogic(forApproach)(onRequest)  // same isOpTypeCase applied to TypeCase can pass in
        } else {
          // if opname is a "isSub" or "isAdd" for older typecase, but we are in newest one? Still send to generic logic
          val pastOps = math.J2.isOps(model.flatten.typeCases)
          if (pastOps.contains(onRequest.request.op)) {
            genericLogic(forApproach)(onRequest)
//          } else if (onRequest.request.op == math.J1.MultBy) {
//            /* Handle MultBy with these data types. */
//            onRequest.tpeCase match {
//              case math.K1.Power => genericLogic(forApproach)(onRequest)
//              case _ => ???
//            }
          } else if (onRequest.request.op == math.J2.Eql) {
            genericLogic(forApproach)(onRequest)
          } else {
            val result = onRequest.tpeCase match {
              case power@math.K1.Power => {
                onRequest.request.op match {
                  case eval@math.M0.Eval =>
                    for {
                      base <- forApproach.dispatch(SendRequest(
                        onRequest.attributes(power.attributes.head),
                        math.J2.getModel.baseDataType,
                        onRequest.request,
                        Some(onRequest)
                      ))
                      exponent <- forApproach.dispatch(SendRequest(
                        onRequest.attributes(power.attributes.tail.head),
                        math.J2.getModel.baseDataType,
                        onRequest.request,
                        Some(onRequest)
                      ))
                      res <- pow(base, exponent)
                    } yield res

                  // NOTE: NO DISPATCHES in mult by....
                  case mp@math.J1.MultBy =>
                    for {

                      other <- forApproach.dispatch(SendRequest(
                        onRequest.request.arguments.toSeq.head._2,   // onRequest.attributes(power.attributes.tail.head),
                        math.J1.getModel.baseDataType,
                        Request(math.M0.Eval, Map.empty),
                        Some(onRequest)
                      ))
                      baseEval <- forApproach.dispatch(SendRequest(
                        onRequest.attributes(power.attributes.head),
                        math.J1.getModel.baseDataType,
                        Request(math.M0.Eval, Map.empty),
                        Some(onRequest)
                      ))
                      // lit(Math.log(this.convert(other).eval()) / Math.log(getBase().eval()))));

                      //eulerNumFixMe <- forApproach.reify(InstanceRep(TypeRep.Double)(2.7182818284590452354))
                      numExpr <- log(baseEval, other)
//                      denomExpr <- log(eulerNumFixMe, baseEval)
//                      fraction <- div(numExpr, denomExpr)
                      addend <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, numExpr)  // fraction)

                      expExpr <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Add,
                        onRequest.attributes(power.attributes.tail.head), addend)

                      res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.K1.Power,
                        onRequest.attributes(power.attributes.head), expExpr)

                    } yield res

                  case _ => ???
                }
              }

              case _ => ???
            }
            result.map(Some(_))
          }
        }
      }
    }

    // newest first
    monoidInstance.combine(k1Provider, j2Provider)
  }
}
