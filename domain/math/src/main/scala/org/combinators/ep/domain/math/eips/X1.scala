package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic, Strings}
import EvolutionImplementationProvider._
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.paradigm.control.Imperative

object X1 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {

    val x1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.X1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      def applicable
      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        (potentialRequest.op == math.M0.Eval) && Set(math.X1.Sub).contains(potentialRequest.tpeCase) ||
          Set(math.X1.PrettyP,math.X1.MultBy).contains(potentialRequest.op) && Set(math.X1.Sub,math.M0.Lit,math.M0.Add).contains(potentialRequest.tpeCase)
      }

      override def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import ffiArithmetic.arithmeticCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import AnyParadigm.syntax._
        assert(applicable(forApproach)(onRequest))

        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.X1.Sub => sub(atts: _*)
                case _ => ???
              }

            case math.M2.PrettyP =>
              onRequest.tpeCase match {
                case math.X1.Sub => makeString(atts, "(", "-", ")")
                case math.M0.Add => makeString(atts, "(", "+", ")")
                case litC@math.M0.Lit =>
                  val att = litC.attributes.head
                  for {
                    ty <- toTargetLanguageType(att.tpe)
                    res <- asString(onRequest.attributes(att), ty)
                  } yield res

                case _ => ???
              }

            case math.X1.MultBy =>
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

                    // if stmt next
                    zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
                    ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(onRequest.attributes.head._2, zero)

                    ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
                      zeroLit <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, zero)
                      res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M1.Sub, zeroLit, resultVar)
                      assignStmt <- ffiImper.imperativeCapabilities.assignVar(resultVar, res)
                      _ <- addBlockDefinitions(Seq(assignStmt))
                    } yield (),
                      Seq.empty
                    )

                    _ <- addBlockDefinitions(Seq(ifStmt))
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
                      math.M2.getModel.baseDataType,
                      onRequest.request
                    ))
                    right <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(rAtt),
                      math.M2.getModel.baseDataType,
                      onRequest.request
                    ))

                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left, right)
                  } yield res
              }

            case _ => ???
          }

        val result =
          for {
            atts <- forEach (onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M3.getModel.baseDataType,
                onRequest.request
              ))
            }
            res <- operate(atts)
          } yield res

        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(x1Provider, M0(paradigm)(ffiArithmetic,ffiStrings))
  }
}
