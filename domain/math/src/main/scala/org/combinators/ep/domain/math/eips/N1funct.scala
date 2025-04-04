package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Functional, control}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object N1funct {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (functional:Functional.WithBase[paradigm.type],
   functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val n1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.N1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m3Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquality.enable()
        } yield ()
      }

      /** PowBy depends on Eval. */

        override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
          val cases = math.N1.getModel.flatten.typeCases
          (potentialRequest.op, potentialRequest.tpeCase) match {
            case (math.N1.PowBy, tpeCase) if cases.contains(tpeCase) => Some(Set(math.M0.Eval))

            case _ => None
          }
        }

      /** PowBy can support any N-ary data type, so prepare for this future eventuality here. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        onRequest.request.op match {
          case math.N1.PowBy => defaultGenericLogic(forApproach)(onRequest)
          case _ => m3Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import functionalControl.functionalCapabilities._
        import functionalControl.lambdaCapabilities._
        import functional.methodBodyCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.tpeCase match {
          //        default Exp<V> powBy(ep.Exp<V> exponent) {
          //          double exponentValue = convert(exponent).eval();
          //          Exp<V> result = this;
          //          for (double counter = Math.floor(Math.abs(exponentValue)); counter > 1; --counter) {
          //            result = mult(result, this);
          //          }
          //          if (exponentValue < 0) {
          //            result = divd(lit(1.0), result);
          //          }
          //          return result;
          //        }

          //          def powBy(self, exp: ep.Exp) {

          //            def powByRec: Double => ep.Exp = (exponentValue: Double) => {
          //               if (1 < exponentValue) {
          //                  mult(self, powByRec(exponentValue - 1))
          //               } else {
          //                  self
          //               }
          //            }
          //            val exponentValue: Double = eval(exp)
          //            if (exponentValue == 0) { return Lit(1) }
          //            else {
          //               val result: ep.Exp = powByRec(Math.floor(Math.abs(exponentValue)))
          //               if (exponentValue < 0) { return divd(lit(1.0), result) }
          //               else { return result }
          //            }
          //          }

          case litC@math.M0.Lit =>
            for {
              resultTpe <- toTargetLanguageType(TypeRep.DataType(math.M2.getModel.baseDataType))
              expName <- freshName(forApproach.names.mangle("exponentValue"))
              expType <- toTargetLanguageType(TypeRep.Double)

              evalExponent <- forApproach.dispatch(SendRequest(
                onRequest.request.arguments.head._2,
                math.M2.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
              ))

              zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
              one <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))

              powByRecTpe <- toTargetLanguageType(TypeRep.Arrow(TypeRep.Double, TypeRep.DataType(math.M2.getModel.baseDataType)))
              powByRecName <- freshName(forApproach.names.mangle("powByRec"))
              powByRecArg <- freshName(forApproach.names.mangle("exponentValue"))
              finalResult <- declareRecursiveVariable(powByRecName, powByRecTpe,
                powByRecVar => lambda(
                  variables = Seq((powByRecArg, expType)),
                  args =>
                    for {
                      recCond <- ffiArithmetic.arithmeticCapabilities.lt(one, args(powByRecArg))
                      result <- ifThenElse(
                        cond = recCond,
                        ifBlock = for {
                          recArg <- ffiArithmetic.arithmeticCapabilities.sub(args(powByRecArg), one)
                          recCall <- methodBodyCapabilities.apply(powByRecVar, Seq(recArg))
                          result <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, recCall)
                        } yield result,
                        elseIfs = Seq.empty,
                        elseBlock = lift(onRequest.selfReference)
                      )
                    } yield result
                )
              )(inBlock =
                powByRecVar => declareVariable(expName, expType, evalExponent)(inBlock = expVar =>
                  for {
                    zeroCond <- ffiEquality.equalityCapabilities.areEqual(expType, expVar, zero)
                    resultName <- freshName(forApproach.names.mangle("result"))
                    result <- ifThenElse(cond = zeroCond,
                      ifBlock = forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, one),
                      elseIfs = Seq.empty,
                      elseBlock = for {
                        absValue <- ffiRealArithmetic.realArithmeticCapabilities.abs(expVar)
                        floorValue <- ffiRealArithmetic.realArithmeticCapabilities.floor(absValue)
                        recursiveCall <- methodBodyCapabilities.apply(powByRecVar, Seq(floorValue))
                        result <- declareVariable(resultName, resultTpe, recursiveCall)(resultVar =>
                          for {
                            ltZeroCond <- ffiArithmetic.arithmeticCapabilities.lt(expVar, zero)
                            result <- ifThenElse(cond = ltZeroCond,
                              ifBlock = for {
                                oneLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, one)
                                divdRes <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Divd, oneLit, resultVar)
                              } yield divdRes,
                              elseIfs = Seq.empty,
                              elseBlock = lift(resultVar)
                            )
                          } yield result

                        )
                      } yield result
                    )
                  } yield result
                )
              )

            } yield Some(finalResult)

          case math.M0.Add | math.M1.Sub =>
            for {
              inner <- forApproach.dispatch(SendRequest(
                onRequest.selfReference,
                math.M2.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
              ))

              innerRes <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, inner)
              res <- forApproach.dispatch(SendRequest(
                innerRes,
                math.M2.getModel.baseDataType,
                onRequest.request
              ))
            } yield Some(res)

          case neg@math.M3.Neg =>
            val lAtt = neg.attributes.head
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(lAtt),
                math.M2.getModel.baseDataType,
                onRequest.request
              ))
              negOne <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))
              innerLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, negOne)

              leftSide <- forApproach.dispatch(SendRequest(
                innerLit,
                math.M2.getModel.baseDataType,
                Request(math.M7.PowBy,  Map(Parameter("other", onRequest.request.op.returnType) -> onRequest.attributes.head._2))  // MUST contain exp some how?
              ))

              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, leftSide, left)
            } yield Some(res)

          case _ =>    // standard example of accessing the generic Logic
            genericLogic(forApproach)(onRequest)

        }
      }
    }

    // newest one must come first
    monoidInstance.combine(n1Provider, m3Provider)
  }
}