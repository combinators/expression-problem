package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.{AnyParadigm, ToTargetLanguageType}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic, Strings}

object I1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val i1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.I1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      def applicable
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (onRequest.request.op == math.I1.MultBy) &&
          (Set(math.M0.Lit, math.M0.Add, math.M1.Sub).contains(onRequest.tpeCase))
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        assert(applicable(forApproach)(onRequest))

        onRequest.tpeCase match {

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
                } yield()
              )
              _ <- addBlockDefinitions(Seq(stmt))

              // if stmt next
              zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
              ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(onRequest.attributes.head._2, zero)

              ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
                  zeroLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, zero)
                  res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M1.Sub, zeroLit, resultVar)
                  assignStmt <-  ffiImper.imperativeCapabilities.assignVar(resultVar, res)
                  _ <- addBlockDefinitions(Seq(assignStmt))
                } yield (),
                Seq.empty
              )

              _ <- addBlockDefinitions(Seq(ifStmt))
            } yield Some(resultVar)

//        default ep.alt1.Exp<V> multBy(ep.Exp<V> other) {
//          return sub(getLeft().multBy(other), getRight().multBy(other));
//        }
          case _ =>
           genericLogic(forApproach)(onRequest)   // standard example of accessing the generic Logic
        }

      }
    }

    // newest first
    monoidInstance.combine(i1Provider, m2Provider)
  }
}
