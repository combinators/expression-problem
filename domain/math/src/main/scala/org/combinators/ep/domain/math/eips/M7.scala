package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings, Trees}

object M7 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m6Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val equalsProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m6Provider.initialize(forApproach)
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
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (Set(math.M7.PowBy).contains(onRequest.request.op))
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._

        val result = onRequest.tpeCase match {
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

          case litC@math.M0.Lit =>
            for {
              expName <- freshName(forApproach.names.mangle("exponentValue"))
              expType <- toTargetLanguageType(TypeRep.Double)

              evalExponent <- forApproach.dispatch(SendRequest(
                onRequest.request.arguments.head._2,
                math.M2.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty),
                Some(onRequest)
              ))
              expValue <- ffiImper.imperativeCapabilities.declareVar(expName, expType, Some(evalExponent))

              varName <- freshName(forApproach.names.mangle("result"))
              baseType <- toTargetLanguageType(onRequest.request.op.returnType)
              resultVar <- ffiImper.imperativeCapabilities.declareVar(varName, baseType, Some(onRequest.selfReference))

              ctrName <- freshName(forApproach.names.mangle("counter"))
              ctrType <- toTargetLanguageType(TypeRep.Double)
              absValue <- ffiRealArithmetic.realArithmeticCapabilities.abs(expValue)
              floorValue <- ffiRealArithmetic.realArithmeticCapabilities.floor(absValue)
              ctrVar <- ffiImper.imperativeCapabilities.declareVar(ctrName, ctrType, Some(floorValue))

              one <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))

              // Know you have add data type so you can construct it
              condExpr <- ffiArithmetic.arithmeticCapabilities.lt(one, ctrVar)
              stmt <- ffiImper.imperativeCapabilities.whileLoop(condExpr, for {
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, resultVar, onRequest.selfReference)
                assignStmt <- ffiImper.imperativeCapabilities.assignVar(resultVar, res)
                decrExpr <- ffiArithmetic.arithmeticCapabilities.sub(ctrVar, one)
                decrStmt <- ffiImper.imperativeCapabilities.assignVar(ctrVar, decrExpr)
                _ <- addBlockDefinitions(Seq(assignStmt, decrStmt))
              } yield()
              )
              _ <- addBlockDefinitions(Seq(stmt))

              // if stmt next
              zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
              ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(onRequest.attributes.head._2, zero)

              ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
                oneLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, one)
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Divd, oneLit, resultVar)
                assignStmt <-  ffiImper.imperativeCapabilities.assignVar(resultVar, res)
                _ <- addBlockDefinitions(Seq(assignStmt))
              } yield (),
                Seq.empty
              )

              _ <- addBlockDefinitions(Seq(ifStmt))
            } yield resultVar

          case math.M0.Add | math.M1.Sub =>
            for {
              inner <- forApproach.dispatch(SendRequest(
                onRequest.selfReference,
                math.M2.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty),
                Some(onRequest)
              ))

              innerRes <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, inner)
              res <- forApproach.dispatch(SendRequest(
                innerRes,
                math.M2.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
            } yield res

          case neg@math.M3.Neg =>
            val lAtt = neg.attributes.head
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(lAtt),
                math.M2.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
              negOne <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))
              innerLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, negOne)

              leftSide <- forApproach.dispatch(SendRequest(
                innerLit,
                math.M2.getModel.baseDataType,
                Request(math.M7.PowBy,  Map(Parameter("other", onRequest.request.op.returnType) -> onRequest.attributes.head._2)),  // MUST contain exp some how?
                Some(onRequest)
              ))

              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, leftSide, left)
            } yield res
          //        default ep.alt1.Exp<V> powBy(ep.Exp<V> other) {
          //          return sub(getLeft().powBy(other), getRight().powBy(other));
          //        }
          case other =>
            val lAtt = other.attributes.head
            if (other.attributes.length == 1) {
              for {
                left <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(lAtt),
                  math.M2.getModel.baseDataType,
                  onRequest.request,
                  Some(onRequest)
                ))
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left)
              } yield res
            } else {
              val rAtt = other.attributes.tail.head

              for {
                left <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(lAtt),
                  math.M2.getModel.baseDataType,
                  onRequest.request,
                  Some(onRequest)
                ))
                right <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(rAtt),
                  math.M2.getModel.baseDataType,
                  onRequest.request,
                  Some(onRequest)
                ))

                res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left, right)
              } yield res
            }
        }
        result.map(Some(_))
      }
    }
    monoidInstance.combine(equalsProvider, m6Provider)
  }
}