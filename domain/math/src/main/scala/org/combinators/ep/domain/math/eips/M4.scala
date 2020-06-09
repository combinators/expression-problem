package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, Lists, Strings}

// Code for M4. Takes adapters for return in if-then-else, s.t. functional- and imperative-style if-then-else can be
// used in an uniform way.
sealed class M4[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {
  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
    Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiLists: Lists.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
     returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
     ifThenElse: IfThenElseCommand
    ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m4Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M4.getModel

      /** Simplify depends upon having a working eval. */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        dt match {
          case math.M0.Lit => Set.empty
          case _  => {
            op match {
              case math.M4.Simplify => Set(math.M0.Eval)
              case _ => Set.empty
            }
          }
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

      def applicable(forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (Set(math.M4.Simplify).contains(onRequest.request.op) &&
          math.M4.getModel.flatten.typeCases.toSet.contains(onRequest.tpeCase)) ||
          Set(math.M4.Collect).contains(onRequest.request.op)  // any foreseeable DT can be collected
      }

      private def collectLogic
          (forApproach: AIP[paradigm.type])
            (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
          import ffiLists.listCapabilities._
          import paradigm._
          import syntax._
          import AnyParadigm.syntax._
          import methodBodyCapabilities._

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
                        math.M4.getModel.baseDataType,
                        Request(math.M4.Collect, Map.empty),
                        Some(onRequest)
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
          import ffiArithmetic.arithmeticCapabilities._
          import paradigm._
          import methodBodyCapabilities._
          import syntax._
          import AnyParadigm.syntax._
          import ffiEquality.equalityCapabilities._
          import ffiBoolean.booleanCapabilities._

          def evalChildren(tpe:DataTypeCase, atts: Map[abstractions.Attribute,Expression]): Generator[MethodBodyContext, List[Expression]] =
            forEach (atts.keys.toSeq) { att:abstractions.Attribute => {
              val expr:Expression = atts.get(att).get
              forApproach.dispatch(
                SendRequest(
                  expr,
                  math.M4.getModel.baseDataType,
                  Request(math.M0.Eval, Map.empty),
                  Some(onRequest)
                )
              )}
            }

          def simplifyRec(att:abstractions.Attribute, attExpr: Expression): Generator[MethodBodyContext, Expression] = {
            forApproach.dispatch(
              SendRequest(
                attExpr,
                math.M4.getModel.baseDataType,
                Request(math.M4.Simplify, Map.empty),
                Some(onRequest)
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
              case math.M1.Sub =>
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
                          res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.M1.Sub, lsimp, rsimp))
                        } yield res
                      )
                  } yield result
                }
              case math.M3.Mult =>
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
                          res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, lsimp, rsimp))
                        } yield res
                      )
                  } yield result
                }
              case math.M3.Divd =>
                vals.flatMap { case List(leftVal, rightVal) =>
                  for {
                    minusOne <- forApproach.reify(InstanceRep(TypeRep.Double)(-1.0))
                    leftEqZero <- areEqual(doubleTy, leftVal, zero)
                    rightEqOne <- areEqual(doubleTy, rightVal, one)
                    leftRightEq <- areEqual(doubleTy, leftVal, rightVal)
                    negRightVal <- mult(minusOne, rightVal)
                    leftRightNeqEq <- areEqual(doubleTy, leftVal, negRightVal)
                    result <-
                      ifThenElse(
                        leftEqZero, returnInIf(zeroLit),
                        Seq(
                          (rightEqOne, returnInIf(simplifyRec(atts.head, attExprs.head))),
                          (leftRightEq, returnInIf(oneLit)),
                          (leftRightNeqEq, returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, minusOne)))
                        ),
                        for {
                          lsimp <- simplifyRec(atts.head, attExprs.head)
                          rsimp <- simplifyRec(atts.tail.head, attExprs.tail.head)
                          res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Divd, lsimp, rsimp))
                        } yield res
                      )
                  } yield result
                }
              case math.M3.Neg =>
                vals.flatMap { case List(innerVal) =>
                  for {
                    innerZero <- areEqual(doubleTy, innerVal, zero)
                    result <-
                      ifThenElse(
                        innerZero, returnInIf(zeroLit),
                        Seq.empty,
                        for {
                          innerSimp <- simplifyRec(atts.head, attExprs.head)
                          res <- returnInIf(forApproach.instantiate(math.M3.getModel.baseDataType, math.M3.Neg, innerSimp))
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

        def logic(forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
          assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)
          onRequest.request.op match {
            case math.M4.Collect => collectLogic(forApproach)(onRequest)
            case math.M4.Simplify => simplifyLogic(forApproach)(onRequest)
            case _ => ???
          }
        }
      }

    // newest one must come first
    monoidInstance.combine(m4Provider, M3(paradigm)(ffiArithmetic, ffiStrings))
  }
}

object M4 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiLists: Lists.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new M4[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

      mkImpl(ffiArithmetic, ffiBoolean, ffiStrings, ffiLists, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiLists: Lists.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    import paradigm.methodBodyCapabilities._
    import imperativeControl.imperativeCapabilities._
    val mkImpl = new M4[paradigm.type, AIP, Unit](paradigm)
    val returnInIf: Generator[paradigm.MethodBodyContext, Expression] => Generator[paradigm.MethodBodyContext, Unit] =
      (expGen) =>
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

    mkImpl(ffiArithmetic, ffiBoolean, ffiStrings, ffiLists, ffiEquality, returnInIf, ite)
  }
}
