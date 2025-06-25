package org.combinators.ep.domain.math.eips.systemI    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.math
import org.combinators.ep.domain.math.systemI
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object I1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiEquals: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val i1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.systemI.I1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = systemI.I1.getModel.flatten.typeCases
        if (potentialRequest.op == systemI.I1.MultBy && cases.contains(potentialRequest.tpeCase)) {
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
          case systemI.I1.MultBy => defaultGenericLogic(forApproach)(onRequest)
          case _ => m2Provider.genericLogic(forApproach)(onRequest)
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

          // Example code that would be generated
          //        default ep.alt1.Exp<V> multBy(ep.Exp<V> other) {
          //          ep.Exp<V> result = other;
          //          for (double counter = Math.floor(Math.abs(getValue())); counter > 1; --counter) {
          //            result = add(result, other);
          //          }
          //          if (getValue() < 0) {
          //            result = sub(lit(0.0), result);
          //          } else if (getvalue() == 0) {
          //            return lit(0.0)
          //          }
          //          return convert(result);
          //        }

          case litC@math.M0.Lit =>
            for {
              varName <- freshName(forApproach.names.mangle("result"))
              baseType <- toTargetLanguageType(onRequest.request.op.returnType)
              resultVar <- ffiImper.imperativeCapabilities.declareVar(varName, baseType, Some(onRequest.request.arguments.head._2))

              ctrName <- freshName(forApproach.names.mangle("counter"))
              ctrType <- toTargetLanguageType(TypeRep.Double)
              absValue <- ffiRealArithmetic.realArithmeticCapabilities.abs(onRequest.attributes.head._2)
              floorValue <- ffiRealArithmetic.realArithmeticCapabilities.floor(absValue)
              ctrVar <- ffiImper.imperativeCapabilities.declareVar(ctrName, ctrType, Some(floorValue))

              one <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))

              // Know you have add data type so you can construct it
              condExpr <- ffiArithmetic.arithmeticCapabilities.lt(one, ctrVar)
              stmt <- ffiImper.imperativeCapabilities.whileLoop(condExpr, for {
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Add, resultVar, onRequest.request.arguments.head._2)
                assignStmt <- ffiImper.imperativeCapabilities.assignVar(resultVar, res)
                decrExpr <- ffiArithmetic.arithmeticCapabilities.sub(ctrVar, one)
                decrStmt <- ffiImper.imperativeCapabilities.assignVar(ctrVar, decrExpr)
                _ <- addBlockDefinitions(Seq(assignStmt, decrStmt))
              } yield ()
              )
              _ <- addBlockDefinitions(Seq(stmt))

              // if (value == 0)
              zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
              ifEqExpr <- ffiEquals.equalityCapabilities.areEqual(ctrType, onRequest.attributes.head._2, zero)

              ifStmtEq <- ffiImper.imperativeCapabilities.ifThenElse(ifEqExpr, for {
                zeroLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, zero)
                assignStmtEq <-  ffiImper.imperativeCapabilities.assignVar(resultVar, zeroLit)
                _ <- addBlockDefinitions(Seq(assignStmtEq))
              } yield (),
                Seq.empty
              )
              _ <- addBlockDefinitions(Seq(ifStmtEq))

              // if (value < >0)
              zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
              ifExpr2 <- ffiArithmetic.arithmeticCapabilities.lt(onRequest.attributes.head._2, zero)

              ifStmt2 <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr2, for {
                zeroLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, zero)
                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M1.Sub, zeroLit, resultVar)
                assignStmt <- ffiImper.imperativeCapabilities.assignVar(resultVar, res)
                _ <- addBlockDefinitions(Seq(assignStmt))
              } yield (),
                Seq.empty
              )

              _ <- addBlockDefinitions(Seq(ifStmt2))
            } yield Some(resultVar)


          // Example code that would be generated
          //        default ep.alt1.Exp<V> multBy(ep.Exp<V> other) {
          //          return sub(getLeft().multBy(other), getRight().multBy(other));
          //        }

          case _ => /* Add and Sub */
            genericLogic(forApproach)(onRequest) // standard example of accessing the generic Logic
        }

      }
    }

    // newest first
    monoidInstance.combine(i1Provider, m2Provider)
  }
}
