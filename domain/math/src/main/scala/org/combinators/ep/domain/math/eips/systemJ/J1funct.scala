package org.combinators.ep.domain.math.eips.systemJ   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}
import org.combinators.ep.domain.abstractions.{DomainTpeRep, Operation}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemJ
import org.combinators.cogen.Command.{Generator, lift}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.cogen.paradigm.control
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J1funct {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m0Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiEquals: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j1Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemJ.J1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m0Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquals.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if ((potentialRequest.op == math.M0.Eval) && Set(math.systemJ.J1.Sub).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else if ((potentialRequest.op == math.systemJ.J1.MultBy) && Set(math.M0.Lit, math.M0.Add, math.systemJ.J1.Sub).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      /** MultBy can support any N-ary data type, so prepare for this future eventuality here. */
      override def genericLogic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        onRequest.request.op match {
          case math.systemJ.J1.MultBy => defaultGenericLogic(forApproach)(onRequest)
          case _ => m0Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import functionalControl.functionalCapabilities._
        import functionalControl.lambdaCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.tpeCase match {

          // Example code that would be generated
          //        default ep.alt1.Exp<V> multBy(ep.Exp<V> other) {
          //          ep.Exp<V> result = other;
          //          for (double counter = Math.floor(Math.abs(getValue())); counter > 1; --counter) {
          //            result = add(result, other);
          //          }
          //          if (getValue() < 0) {
          //            result = sub(lit(0.0), result);
          //          }
          //          return convert(result);
          //        }

          //            def multByRec: Double => ep.Exp = (multiplier: Double) => {
          //               if (1 < multiplier) {
          //                  add(self, powByRec(multiplier - 1))
          //               } else {
          //                  self
          //               }
          //            }
          //            val multiplier: Double = eval(exp)
          //            if (multiplier == 0) { return Lit(0) }
          //            else {
          //               val result: ep.Exp = multByRec(Math.floor(Math.abs(multiplier)))
          //               if (multiplier < 0) { return sub(lit(0.0), result) }
          //               else { return result }
          //            }
          //          }


          case litC@math.M0.Lit =>
            for {
              resultTpe <- toTargetLanguageType(DomainTpeRep.DataType(math.M2.getModel.baseDataType))
              multName <- freshName(forApproach.names.mangle("multiplier"))
              multType <- toTargetLanguageType(TypeRep.Double)

              evalMultiplier <- forApproach.dispatch(SendRequest(
                onRequest.request.arguments.head._2,
                math.M2.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
              ))

              zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
              one <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))

              multByRecTpe <- toTargetLanguageType(TypeRep.Arrow(TypeRep.Double, DomainTpeRep.DataType(math.M2.getModel.baseDataType)))
              multByRecName <- freshName(forApproach.names.mangle("multByRec"))
              multByRecArg <- freshName(forApproach.names.mangle("multiplier"))
              finalResult <- declareRecursiveVariable(multByRecName, multByRecTpe,
                powByRecVar => lambda(
                  variables = Seq((multByRecArg, multType)),
                  args =>
                    for {
                      recCond <- ffiArithmetic.arithmeticCapabilities.lt(one, args(multByRecArg))
                      result <- ifThenElse(
                        cond = recCond,
                        ifBlock = for {
                          recArg <- ffiArithmetic.arithmeticCapabilities.sub(args(multByRecArg), one)
                          recCall <- methodBodyCapabilities.apply(powByRecVar, Seq(recArg))
                          result <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Add, onRequest.selfReference, recCall)
                        } yield result,
                        elseIfs = Seq.empty,
                        elseBlock = lift(onRequest.selfReference)
                      )
                    } yield result
                )
              )(inBlock =
                multByRecVar => declareVariable(multName, multType, evalMultiplier)(inBlock = expVar =>
                  for {
                    zeroCond <- ffiEquals.equalityCapabilities.areEqual(multType, expVar, zero)
                    resultName <- freshName(forApproach.names.mangle("result"))
                    result <- ifThenElse(cond = zeroCond,
                      ifBlock = forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, zero),
                      elseIfs = Seq.empty,
                      elseBlock = for {
                        absValue <- ffiRealArithmetic.realArithmeticCapabilities.abs(expVar)
                        floorValue <- ffiRealArithmetic.realArithmeticCapabilities.floor(absValue)
                        recursiveCall <- methodBodyCapabilities.apply(multByRecVar, Seq(floorValue))
                        result <- declareVariable(resultName, resultTpe, recursiveCall)(resultVar =>
                          for {
                            ltZeroCond <- ffiArithmetic.arithmeticCapabilities.lt(expVar, zero)
                            result <- ifThenElse(cond = ltZeroCond,
                              ifBlock = for {
                                zeroLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, zero)
                                divdRes <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M1.Sub, zeroLit, resultVar)
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


          // Example code that would be generated
          //        default ep.alt1.Exp<V> multBy(ep.Exp<V> other) {
          //          return sub(getLeft().multBy(other), getRight().multBy(other));
          //        }
          case subC@math.systemJ.J1.Sub =>
            if (onRequest.request.op == math.M0.Eval) {
              for {
                left <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(subC.attributes.head),
                  math.systemJ.J1.getModel.baseDataType,
                  onRequest.request
                ))
                right <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(subC.attributes.tail.head),
                  math.systemJ.J1.getModel.baseDataType,
                  onRequest.request
                ))
                res <- sub(left, right)
              } yield Some(res)
            } else {
              genericLogic(forApproach)(onRequest) // standard example of accessing the generic Logic
            }

          case _ => /* Add */
            genericLogic(forApproach)(onRequest) // standard example of accessing the generic Logic
        }
      }
    }

    // newest first
    monoidInstance.combine(m0Provider, j1Provider)
  }
}
