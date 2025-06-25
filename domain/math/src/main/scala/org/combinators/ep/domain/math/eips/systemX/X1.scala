package org.combinators.ep.domain.math.eips.systemX     /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemX
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object X1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m0Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiEquals: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {

    val x1Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemX.X1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m0Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquals.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if ((potentialRequest.op == math.M0.Eval) && Set(systemX.X1.Sub).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else if (Set(systemX.X1.MultBy, systemX.X1.PrettyP).contains(potentialRequest.op) && Set(systemX.X1.Sub, math.M0.Lit, math.M0.Add).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      override def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        def operate(attGenerators: Seq[Generator[paradigm.MethodBodyContext, syntax.Expression]]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case systemX.X1.Sub => 
                  for {
                    atts <- forEach(attGenerators)(g => g)
                    result <- sub(atts*)
                  } yield result
                  
                case _ => ???
              }

            case systemX.X1.PrettyP =>
              onRequest.tpeCase match {
                case systemX.X1.Sub =>
                  for {
                    atts <- forEach(attGenerators)(g => g)
                    result <- makeString(atts, "(", "-", ")")
                  } yield  result
                case math.M0.Add =>
                  for {
                    atts <- forEach(attGenerators)(g => g)
                    result <- makeString(atts, "(", "+", ")")
                  } yield result
                case litC@math.M0.Lit =>
                  val att = litC.attributes.head
                  for {
                    ty <- toTargetLanguageType(att.tpe)
                    res <- asString(onRequest.attributes(att), ty)
                  } yield res

                case _ => ???
              }

            case systemX.X1.MultBy =>
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
                  } yield resultVar

                //        default ep.alt1.Exp<V> multBy(ep.Exp<V> other) {
                //          return sub(getLeft().multBy(other), getRight().multBy(other));
                //        }
                case other => // handles nearly every method with two operands
                  val lAtt = other.attributes.head
                  val rAtt = other.attributes.tail.head

                  for {
                    left <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(lAtt),
                      math.M0.getModel.baseDataType,
                      onRequest.request
                    ))
                    right <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(rAtt),
                      math.M0.getModel.baseDataType,
                      onRequest.request
                    ))

                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left, right)
                  } yield res
              }

            case _ => ???
          }

        val attGenerators = onRequest.tpeCase.attributes.map { att =>
          forApproach.dispatch(SendRequest(
            onRequest.attributes(att),
            math.M0.getModel.baseDataType,
            onRequest.request
          ))
        }
        
        val result =
          for {
            res <- operate(attGenerators)
          } yield res

        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(x1Provider, m0Provider)
  }
}
