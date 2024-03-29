package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}

object J1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (m0Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.J1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m0Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      /** Doesn't need Eval, because knows about Lit and how to get values directly. */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        Set.empty
      }

      def applicable
      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        (potentialRequest.op == math.J1.MultBy && Set(math.M0.Lit, math.M0.Add, math.J1.Sub).contains(potentialRequest.tpeCase)) ||
          (potentialRequest.op == math.M0.Eval) && Set(math.J1.Sub).contains(potentialRequest.tpeCase)
      }

      /** MultBy can support any N-ary data type, so prepare for this future eventuality here. */
      override def genericLogic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        onRequest.request.op match {
          case math.J1.MultBy => defaultGenericLogic(forApproach)(onRequest)
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

        assert(applicable(forApproach)(onRequest))

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

          case litC@math.M0.Lit =>
            for {
              varName <- freshName(forApproach.names.mangle("result"))
              baseType <- toTargetLanguageType(onRequest.request.op.returnType)
              resultVar <- ffiImper.imperativeCapabilities.declareVar(varName, baseType, Some(onRequest.request.arguments.head._2))

              ctrName <- freshName(forApproach.names.mangle("counter"))
              ctrType <- toTargetLanguageType(TypeRep.Double)
              absValue <- ffiRealArithmetic.realArithmeticCapabilities.abs(onRequest.attributes.head._2)   // on Request is a LIT so just get value
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


          // Example code that would be generated
          //        default ep.alt1.Exp<V> multBy(ep.Exp<V> other) {
          //          return sub(getLeft().multBy(other), getRight().multBy(other));
          //        }
          case subC@math.J1.Sub =>
            if (onRequest.request.op == math.M0.Eval) {
              for {
                left <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(subC.attributes.head),
                  math.M1.getModel.baseDataType,
                  onRequest.request
                ))
                right <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(subC.attributes.tail.head),
                  math.M1.getModel.baseDataType,
                  onRequest.request
                ))
                res <- sub(left, right)
              } yield Some(res)
            } else {
              genericLogic(forApproach)(onRequest)   // standard example of accessing the generic Logic
            }

          case _ => /* Add */
            genericLogic(forApproach)(onRequest)   // standard example of accessing the generic Logic
        }
      }
    }

    // newest first
    monoidInstance.combine(m0Provider, j1Provider)
  }
}