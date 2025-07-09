package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, Parameter}
import org.combinators.ep.domain.math
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}

object M7 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m6Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiEquals: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper:control.Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val equalsProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M7.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m6Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiEquals.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.M7.getModel.flatten.typeCases
        if (potentialRequest.op == math.M7.PowBy && cases.contains(potentialRequest.tpeCase)) {
          potentialRequest.tpeCase match {
            case math.M3.Neg => Some(Set.empty)
            case _ => Some(Set(math.M0.Eval))
          }
        } else {
          None
        }
      }

      /** PowBy can support any N-ary data type, so prepare for this future eventuality here. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        onRequest.request.op match {
          case math.M7.PowBy => defaultGenericLogic(forApproach)(onRequest)
          case _ => m6Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
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

          case litC@math.M0.Lit =>
            for {
              expName <- freshName(forApproach.names.mangle("exponentValue"))
              expType <- toTargetLanguageType(TypeRep.Double)

              evalExponent <- forApproach.dispatch(SendRequest(
                onRequest.request.arguments.head._2,
                math.M2.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
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
              ifEqExpr <- ffiEquals.equalityCapabilities.areEqual(expType, expValue, zero)

              ifStmtEq <- ffiImper.imperativeCapabilities.ifThenElse(ifEqExpr, for {
                oneLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, one)
                assignStmtEq <-  ffiImper.imperativeCapabilities.assignVar(resultVar, oneLit)
                _ <- addBlockDefinitions(Seq(assignStmtEq))
              } yield (),
                Seq.empty
              )

              _ <- addBlockDefinitions(Seq(ifStmtEq))
              ifLtExpr <- ffiArithmetic.arithmeticCapabilities.lt(expValue, zero)

              ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifLtExpr, for {
                oneLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, one)
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Divd, oneLit, resultVar)
                assignStmt <-  ffiImper.imperativeCapabilities.assignVar(resultVar, res)
                _ <- addBlockDefinitions(Seq(assignStmt))
              } yield (),
                Seq.empty
              )
              _ <- addBlockDefinitions(Seq(ifStmt))
            } yield Some(resultVar)

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
    monoidInstance.combine(equalsProvider, m6Provider)
  }
}

