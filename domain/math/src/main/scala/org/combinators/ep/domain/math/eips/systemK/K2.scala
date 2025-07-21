package org.combinators.ep.domain.math.eips.systemK    /*DD:LI:AI*/

import org.combinators.cogen.{Command, InstanceRep, TypeRep}
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans, Equality, Lists, Strings}
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.math.systemK
import org.combinators.ep.domain.{GenericModel, abstractions, math}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

// Takes adapters for return in if-then-else, s.t. functional- and imperative-style if-then-else can be
// used in an uniform way.
sealed class K2[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {
  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply
  (k1Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiLists: Lists.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
   ifThenElse: IfThenElseCommand
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val k2Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = systemK.K2.getModel

      // K2 adds "k2", Seq.empty, Seq(Simplify, Collect))

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = systemK.K2.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          potentialRequest.op match {
            case systemK.K2.Simplify => Some(Set(math.M0.Eval))
            case systemK.K2.Collect => Some(Set.empty)
            case _ =>  None
          }
        } else {
          None
        }
      }

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiStrings.enable()
          _ <- ffiLists.enable()
          _ <- ffiEquality.enable()
        } yield ()
      }

      private def collectLogic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiLists.listCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import syntax._

        def collectOp(innerListTy: Type): Generator[MethodBodyContext, Expression] = {
          val atts = onRequest.tpeCase.attributes.map(onRequest.attributes)

          onRequest.tpeCase match {
            case math.M0.Lit => create(innerListTy, atts)
            case _ =>
              for {
                collectedResults <- forEach(onRequest.attributes.toSeq) {

                  attExpr => {
                    val att = attExpr._1
                    val expr = attExpr._2
                    forApproach.dispatch(
                      SendRequest(
                        expr,
                        systemK.K2.getModel.baseDataType,
                        Request(systemK.K2.Collect, Map.empty)
                      )
                    )}
                }
                res <- collectedResults.tail.foldLeft(Command.lift[MethodBodyContext, Expression](collectedResults.head)) { case (lastRes, nextCol) =>
                  for {
                    lastExp <- lastRes
                    nextRes <- append(lastExp, nextCol)
                  } yield nextRes
                }
              } yield res
          }
        }

        for {
          listDoubleTy <- toTargetLanguageType(onRequest.request.op.returnType)
          _ <- forApproach.resolveAndAddImport(listDoubleTy)
          innerTy <- toTargetLanguageType(onRequest.request.op.returnType.asInstanceOf[TypeRep.Sequence[Double]].elemTpe)
          _ <- forApproach.resolveAndAddImport(innerTy)
          result <- collectOp(innerTy)
        } yield Some(result)
      }

      private def simplifyLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiBoolean.booleanCapabilities._
        import ffiEquality.equalityCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import syntax._

        def evalChildren(tpe:DataTypeCase, atts: Map[abstractions.Attribute,Expression]): Generator[MethodBodyContext, List[Expression]] =
          forEach (atts.keys.toSeq) { (att:abstractions.Attribute) => {
            val expr:Expression = atts(att)
            forApproach.dispatch(
              SendRequest(
                expr,
                math.systemJ.J3.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
              )
            )}
          }

        def simplifyRec(att:abstractions.Attribute, attExpr: Expression): Generator[MethodBodyContext, Expression] = {
          forApproach.dispatch(
            SendRequest(
              attExpr,
              systemK.K2.getModel.baseDataType,
              Request(systemK.K2.Simplify, Map.empty)
            )
          )
        }

        def simplifyOp(
                        atts:Seq[abstractions.Attribute],
                        attExprs: Seq[Expression],
                        vals: Generator[MethodBodyContext, List[Expression]],
                        doubleTy: Type,
                        zero: Expression,
                        one: Expression,
                        zeroLit: Generator[MethodBodyContext, Expression],
                        oneLit: Generator[MethodBodyContext, Expression]
                      ): Generator[MethodBodyContext, Option[Expression]] = {
          onRequest.tpeCase match {
            case math.M0.Lit => Command.lift(Some(onRequest.selfReference))
            case math.M0.Add =>
              vals.flatMap { case List(leftVal, rightVal) =>
                for {
                  addVals <- add(leftVal, rightVal)
                  addValZero <- areEqual(doubleTy, addVals, zero)
                  leftEqZero <- areEqual(doubleTy, leftVal, zero)
                  rightEqZero <- areEqual(doubleTy, rightVal, zero)
                  result <-
                    ifThenElse(
                      addValZero, returnInIf(zeroLit),
                      Seq(
                        (leftEqZero, returnInIf(simplifyRec(atts.tail.head, attExprs.tail.head))),
                        (rightEqZero, returnInIf(simplifyRec(atts.head, attExprs.head)))
                      ),
                      for {
                        lsimp <- simplifyRec(atts.tail.head, attExprs.head)
                        rsimp <- simplifyRec(atts.head, attExprs.tail.head)
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Add, lsimp, rsimp))
                      } yield res
                    )
                } yield result
              }
            case math.systemJ.J1.Sub =>
              vals.flatMap { case List(leftVal, rightVal) =>
                for {
                  lrEq <- areEqual(doubleTy, leftVal, rightVal)
                  result <-
                    ifThenElse(
                      lrEq, returnInIf(zeroLit),
                      Seq.empty,
                      for {
                        lsimp <- simplifyRec(atts.head, attExprs.head)
                        rsimp <- simplifyRec(atts.tail.head, attExprs.tail.head)
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.systemJ.J1.Sub, lsimp, rsimp))
                      } yield res
                    )
                } yield result
              }
            case math.systemJ.J2.Mult =>
              vals.flatMap { case List(leftVal, rightVal) =>
                for {
                  leftEqZero <- areEqual(doubleTy, leftVal, zero)
                  rightEqZero <- areEqual(doubleTy, rightVal, zero)
                  anyEqZero <- or(Seq(leftEqZero, rightEqZero))
                  leftEqOne <- areEqual(doubleTy, leftVal, one)
                  rightEqOne <- areEqual(doubleTy, rightVal, one)
                  result <-
                    ifThenElse(
                      anyEqZero, returnInIf(zeroLit),
                      Seq(
                        (leftEqOne, returnInIf(simplifyRec(atts.tail.head, attExprs.tail.head))),
                        (rightEqOne, returnInIf(simplifyRec(atts.head, attExprs.head)))
                      ),
                      for {
                        lsimp <- simplifyRec(atts.head, attExprs.head)
                        rsimp <- simplifyRec(atts.tail.head, attExprs.tail.head)
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.systemJ.J2.Mult, lsimp, rsimp))
                      } yield res
                    )
                } yield result
              }

            case math.systemK.K1.Power =>
              vals.flatMap { case List(leftVal, rightVal) =>
                for {
                  rightEqZero <- areEqual(doubleTy, rightVal, zero)
                  leftEqOne <- areEqual(doubleTy, leftVal, one)

                  result <-
                    ifThenElse(
                      rightEqZero, returnInIf(oneLit),      // raising to Zero-th power!
                      Seq(
                        (leftEqOne, returnInIf(oneLit)),          // one to any power is 1
                      ),
                      for {                                       // default
                        lsimp <- simplifyRec(atts.head, attExprs.head)
                        rsimp <- simplifyRec(atts.tail.head, attExprs.tail.head)
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.systemK.K1.Power, lsimp, rsimp))
                      } yield res
                    )
                } yield result
              }

            case other => throw new NotImplementedError(other.toString)
          }
        }

        val atts = onRequest.tpeCase.attributes.map(onRequest.attributes)
        for {
          doubleTy <- toTargetLanguageType(TypeRep.Double)
          zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0.0))
          one <- forApproach.reify(InstanceRep(TypeRep.Double)(1.0))
          zeroLit = forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, zero)
          oneLit = forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, one)
          result <- simplifyOp(onRequest.attributes.keys.toSeq, atts, evalChildren(onRequest.tpeCase, onRequest.attributes), doubleTy, zero, one, zeroLit, oneLit)
        } yield result
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        onRequest.request.op match {
          case systemK.K2.Collect => collectLogic(forApproach)(onRequest)
          case _ => k1Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic(forApproach: AIP[paradigm.type])
               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {
          case systemK.K2.Collect => genericLogic(forApproach)(onRequest)
          case systemK.K2.Simplify => simplifyLogic(forApproach)(onRequest)
          case _ => ???
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(k2Provider, k1Provider)
  }
}



object K2 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (k1Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiLists: Lists.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new K2[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(k1Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiLists, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (k1Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (imperativeControl: control.Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiLists: Lists.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import imperativeControl.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._
    import paradigm.syntax._
    val mkImpl = new K2[paradigm.type, AIP, Unit](paradigm)
    val returnInIf: Generator[paradigm.MethodBodyContext, Expression] => Generator[paradigm.MethodBodyContext, Unit] =
      expGen =>
        for {
          resultExp <- expGen
          resultStmt <- returnStmt(resultExp)
          _ <- addBlockDefinitions(Seq(resultStmt))
        } yield None

    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          resultStmt <- ifThenElse(cond, ifBlock, ifElseBlocks, Some(elseBlock))
          _ <- addBlockDefinitions(Seq(resultStmt))
        } yield None

    mkImpl(k1Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiLists, ffiEquality, returnInIf, ite)
  }
}
