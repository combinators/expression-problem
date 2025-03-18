package org.combinators.ep.domain.math.eips.systemJ   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Operation, Parameter, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemJ
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.{AnyParadigm, control}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J6funct {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j5Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquals: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j6Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemJ.J6.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j5Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquals.enable()
        } yield ()
      }

      // J6 adds  Seq.empty, Seq(PowBy))
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {

        val cases = systemJ.J6.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          potentialRequest.op match {
            case systemJ.J6.PowBy => Some(Set(math.M0.Eval))
            case _ => None
          }
        } else {
          None
        }
      }
      //      /** PowBy depends on Eval. */
      //      override def dependencies(op:Operation, dt:DataTypeCase) : Option[Set[Operation]] = {
      //        op match {
      //          case math.J6.PowBy => Some(Set(math.M0.Eval))
      //          case _ => None
      //        }
      //      }
      //
      //      def applicable
      //        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
      //        Set(math.J6.PowBy).contains(potentialRequest.op)
      //      }

      /** PowBy can support any N-ary data type, so prepare for this future eventuality here. */
      override def genericLogic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        onRequest.request.op match {
          case systemJ.J6.PowBy => defaultGenericLogic(forApproach)(onRequest)
          case _ => j5Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import ffiArithmetic.arithmeticCapabilities._
        import functionalControl.functionalCapabilities._
        import functionalControl.lambdaCapabilities._

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
                    zeroCond <- ffiEquals.equalityCapabilities.areEqual(expType, expVar, zero)
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


          case math.M0.Add | systemJ.J1.Sub =>
            for {
              inner <- forApproach.dispatch(SendRequest(
                onRequest.selfReference,
                systemJ.J6.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
              ))

              innerRes <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, inner)
              res <- forApproach.dispatch(SendRequest(
                innerRes,
                systemJ.J6.getModel.baseDataType,
                onRequest.request
              ))
            } yield Some(res)

          case neg@math.systemJ.J3.Neg =>
            val lAtt = neg.attributes.head
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(lAtt),
                systemJ.J6.getModel.baseDataType,
                onRequest.request
              ))
              negOne <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))
              innerLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, negOne)

              leftSide <- forApproach.dispatch(SendRequest(
                innerLit,
                systemJ.J6.getModel.baseDataType,
                Request(systemJ.J6.PowBy, Map(Parameter("other", onRequest.request.op.returnType) -> onRequest.attributes.head._2)) // MUST contain exp some how?
              ))

              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.systemJ.J2.Mult, leftSide, left)
            } yield Some(res)

          case _ => // standard example of accessing the generic Logic
            genericLogic(forApproach)(onRequest)

        }
      }
    }
    // newest one must come first
    monoidInstance.combine(j6Provider, j5Provider)
  }
}
