package org.combinators.ep.domain.math.eips.systemK   /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans, RealArithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.{systemJ, systemK}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object K1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val k1Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemK.K1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      // K1 adds Seq(Power), J2.isOps(Seq(Power)))
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = systemK.K1.getModel.flatten.typeCases
        (potentialRequest.op, potentialRequest.tpeCase) match {

          case (math.systemJ.J2.Eql, math.systemK.K1.Power) => Some(Set(math.systemJ.J2.isOp(systemK.K1.Power)))
          case (math.M0.Eval, math.systemK.K1.Power) => Some(Set.empty)

          // isInv => empty for any non inv argument (returns false), eql for inv argument (left and right eql)
          case (isOp, tpeCase) if isOp == math.systemJ.J2.isOp(systemK.K1.Power) => Some(if (isOp == math.systemJ.J2.isOp(tpeCase)) Set(math.systemJ.J2.Eql) else Set.empty)
          // isXXX for inv argument => empty, e.g. isAdd(inv) = false
          case (isOp, systemK.K1.Power) if math.systemJ.J2.isOps(cases).contains(isOp) => Some(Set.empty)

          case (math.systemJ.J1.MultBy, systemK.K1.Power) => Some(Set(math.M0.Eval))   // required for this implementation

          case (op, systemK.K1.Power) if math.systemJ.J2.getModel.flatten.ops.contains(op) => Some(Set.empty)

          case (_, _) => None
        }
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

        import ffiRealArithmetic.realArithmeticCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        // need to know when isOp is not in the onRequest Type (to handle return false; default implementation)
        // because then we can return FALSE
        if (onRequest.request.op != math.systemJ.J2.isOp(onRequest.tpeCase) && onRequest.request.op.tags.contains(math.systemJ.J2.IsOp)) {
          import ffiBoolean.booleanCapabilities._
          for {
            booleanFalse <- falseExp
          } yield Some(booleanFalse)
        } else if (onRequest.request.op == math.systemJ.J2.isOp(onRequest.tpeCase)) {
          genericLogic(forApproach)(onRequest) // same isOpTypeCase applied to TypeCase can pass in
        } else {
          // if opname is a "isSub" or "isAdd" for older typecase, but we are in newest one? Still send to generic logic
          val pastOps = math.systemJ.J2.isOps(model.flatten.typeCases)
          if (pastOps.contains(onRequest.request.op)) {
            genericLogic(forApproach)(onRequest)
            //          } else if (onRequest.request.op == math.J1.MultBy) {
            //            /* Handle MultBy with these data types. */
            //            onRequest.tpeCase match {
            //              case math.K1.Power => genericLogic(forApproach)(onRequest)
            //              case _ => ???
            //            }
          } else if (onRequest.request.op == math.systemJ.J2.Eql) {
            genericLogic(forApproach)(onRequest)
          } else {
            val result = onRequest.tpeCase match {
              case power@systemK.K1.Power => {
                onRequest.request.op match {
                  case eval@math.M0.Eval =>
                    for {
                      base <- forApproach.dispatch(SendRequest(
                        onRequest.attributes(power.attributes.head),
                        math.systemJ.J2.getModel.baseDataType,
                        onRequest.request
                      ))
                      exponent <- forApproach.dispatch(SendRequest(
                        onRequest.attributes(power.attributes.tail.head),
                        math.systemJ.J2.getModel.baseDataType,
                        onRequest.request
                      ))
                      res <- pow(base, exponent)
                    } yield res

                  // NOTE: NO DISPATCHES in mult by....
                  case mp@math.systemJ.J1.MultBy =>
                    for {

                      other <- forApproach.dispatch(SendRequest(
                        onRequest.request.arguments.toSeq.head._2, // onRequest.attributes(power.attributes.tail.head),
                        math.systemK.K1.getModel.baseDataType,
                        Request(math.M0.Eval, Map.empty)
                      ))
                      baseEval <- forApproach.dispatch(SendRequest(
                        onRequest.attributes(power.attributes.head),
                        math.systemK.K1.getModel.baseDataType,
                        Request(math.M0.Eval, Map.empty)
                      ))
                      // lit(Math.log(this.convert(other).eval()) / Math.log(getBase().eval()))));

                      /***

                      double value = other.<Double>accept(this.makeEval());
                      double num = Math.log(value);
                      return new Power(exp.getLeft(),
                           new Add(exp.getRight(), new Lit     (num / Math.log(exp.getLeft().<Double>accept(this.makeEval())))));
                           ^^^^ is expExpr         ^^^ addend   ^^^ numExpr
                       **/

                      //eulerNumFixMe <- forApproach.reify(InstanceRep(TypeRep.Double)(2.7182818284590452354))
                      numExpr <- log(baseEval, other)
                      //                      denomExpr <- log(eulerNumFixMe, baseEval)
                      //                      fraction <- div(numExpr, denomExpr)
                      addend <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, numExpr) // fraction)

                      expExpr <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Add,
                        onRequest.attributes(power.attributes.tail.head), addend)

                      res <- forApproach.instantiate(math.M0.getModel.baseDataType, systemK.K1.Power,
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
