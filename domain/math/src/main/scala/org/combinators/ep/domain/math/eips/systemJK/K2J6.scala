package org.combinators.ep.domain.math.eips.systemJK    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math.systemJK
import org.combinators.ep.domain.{GenericModel, abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

/** Upon merging M7 and I2 there is a need for MultByx(Divd, Mult, Neg) as well as a need for
  * (Collect,Simplify,Id,AsTree,Equals,PowBy)xPower
  *
  * These all have to be captured here...
  */
sealed class K2J6[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (j6Provider: EvolutionImplementationProvider[AIP[paradigm.type]], k2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
        ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
        returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
        ifThenElse: IfThenElseCommand):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val k2j6Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = systemJK.K2J6.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiEquality.enable()

          _ <- j6Provider.initialize(forApproach)
          _ <- k2Provider.initialize(forApproach)
        } yield ()
      }

      // k1 <- k2
      //   K1(Seq(Power), J2.isOps(Seq(Power)))), K2("k2", Seq.empty, Seq(Simplify, Collect)))

      // j1 <- j2 <- j3 <- j4 <- j5 <- j6
      // J1    Seq(Sub), Seq(MultBy))
      // J2    Seq(Mult), Seq(Eql)  ++ isOps(allTypes))
      // J3    Seq(Neg, Divd), Seq(PrettyP) ++ J2.isOps(Seq(Neg,Divd)))
      // J4    ("j4", Seq.empty, Seq(Operation.asTree, Identifier))
      // J5    Seq.empty, Seq(Equals))
      // J6    Seq.empty, Seq(PowBy))
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = systemJK.K2J6.getModel.flatten.typeCases
        (potentialRequest.op, potentialRequest.tpeCase) match {
          case (math.systemK.K2.Simplify, math.systemJ.J2.Mult) => Some(Set(math.M0.Eval))
          case (math.systemK.K2.Simplify, math.systemJ.J3.Neg) => Some(Set(math.M0.Eval))
          case (math.systemK.K2.Simplify, math.systemJ.J3.Divd) => Some(Set(math.M0.Eval))

          case (math.systemK.K2.Collect, math.systemJ.J2.Mult) => Some(Set.empty)
          case (math.systemK.K2.Collect, math.systemJ.J3.Neg) => Some(Set.empty)
          case (math.systemK.K2.Collect, math.systemJ.J3.Divd) => Some(Set.empty)

          // do not handle MultBy Lit (special)
          //case (math.J1.MultBy, tpeCase) if cases.contains(tpeCase) && !Set(math.M0.Lit).contains(tpeCase) => Some(Set.empty)
          case (math.systemJ.J1.MultBy, math.systemK.K1.Power) => Some(Set.empty)
          case (math.systemJ.J3.PrettyP, math.systemK.K1.Power) => Some(Set.empty)

          case (math.systemJ.J4.Identifier, math.systemK.K1.Power) => Some(Set.empty)
          case (math.systemJ.J6.PowBy, math.systemK.K1.Power) => Some(Set.empty)  // optimized
          case (Operation.asTree, math.systemK.K1.Power) => Some(Set(math.systemJ.J4.Identifier))
          case (math.systemJ.J5.Equals, math.systemK.K1.Power) => Some(Set(Operation.asTree))
          case (math.systemJ.J2.Eql, math.systemK.K1.Power) => Some(Set(math.systemJ.J2.isOp(math.systemK.K1.Power)))

          // isPower => empty for any non power argument (returns false), eql for power argument (left and right eql)
          case (isOp, tpeCase) if isOp == math.systemJ.J2.isOp(math.systemK.K1.Power) => Some(if (isOp == math.systemJ.J2.isOp(tpeCase)) Set(math.systemJ.J2.Eql) else Set.empty)
          // isXXX for power argument => empty, e.g. isAdd(power) = false
          case (isOp, math.systemK.K1.Power) if math.systemJ.J2.isOps(cases).contains(isOp) => Some(Set.empty)
          // rest handled above by first two cases
          case (_, _) => None
        }
      }

      // Simplify of Power -- if exponent is 1, then ignore! If exponent is 0, turn to 1; if exponent is -1, turn to DivD
      private def simplifyLogic(forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
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
                math.M0.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
              )
            )}
          }

        def simplifyRec(att:abstractions.Attribute, attExpr: Expression): Generator[MethodBodyContext, Expression] = {
          forApproach.dispatch(
            SendRequest(
              attExpr,
              math.M0.getModel.baseDataType,
              Request(math.systemK.K2.Simplify, Map.empty)
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
          import ffiArithmetic.arithmeticCapabilities._
          import ffiBoolean.booleanCapabilities._
          onRequest.tpeCase match {
            case math.systemJ.J3.Divd =>
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
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.systemJ.J3.Divd, lsimp, rsimp))
                      } yield res
                    )
                } yield result
              }
            case math.systemJ.J3.Neg =>
              vals.flatMap { case List(innerVal) =>
                for {
                  innerZero <- areEqual(doubleTy, innerVal, zero)
                  result <-
                    ifThenElse(
                      innerZero, returnInIf(zeroLit),
                      Seq.empty,
                      for {
                        innerSimp <- simplifyRec(atts.head, attExprs.head)
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.systemJ.J3.Neg, innerSimp))
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

      // should be no need to define genericLogic since (by default) it will go through the chain of past providers...
      override def genericLogic
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        try {
          j6Provider.genericLogic(forApproach)(onRequest)
        } catch {
          case _:RuntimeException | _:NotImplementedError => k2Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        import AnyParadigm.syntax._
        import ffiStrings.stringCapabilities._            // DON'T BE FOOLED. NEEDED
        import paradigm._
        import methodBodyCapabilities._                   // DON'T BE FOOLED. NEEDED

        onRequest.request.op match {
          case math.systemJ.J5.Equals => genericLogic(forApproach)(onRequest)
          case math.systemJ.J2.Eql => genericLogic(forApproach)(onRequest)

          // handle MultBy for Power by using logs
          // for example, 3^4 * 7 = 567
          //   note that log_3(7) = 1.7712437491614222600679283070825
          //   thus Power(3, 4) turns into (Power(3, 5.7712437491614222600679283070825)
          // HOWEVER this can only work if you introduce a data type LOG
          // instead, where we are, we can turn this into a Mult
          case math.systemJ.J1.MultBy => //j6Provider.genericLogic(forApproach)(onRequest)
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.systemJ.J2.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case op if op == math.systemJ.J2.isOp(onRequest.tpeCase) => j6Provider.genericLogic(forApproach)(onRequest)
          case op if op != math.systemJ.J2.isOp(onRequest.tpeCase) && op.tags.contains(math.systemJ.J2.IsOp) => j6Provider.genericLogic(forApproach)(onRequest)

          case math.systemJ.J3.PrettyP =>
            onRequest.tpeCase match {
              case math.systemK.K1.Power =>
                for {
                  atts <- forEach(onRequest.tpeCase.attributes) { att =>
                    forApproach.dispatch(SendRequest(
                      onRequest.attributes(att),
                      math.systemJ.J3.getModel.baseDataType,
                      onRequest.request
                    ))
                  }
                  res <- makeString(atts, "(", "^", ")")
                } yield Some(res)
              case _ => ???
            }
          case math.systemK.K2.Simplify => simplifyLogic(forApproach)(onRequest)

          case op if op == Operation.asTree => genericLogic(forApproach)(onRequest)
          case math.systemJ.J4.Identifier => genericLogic(forApproach)(onRequest)
          case math.systemK.K2.Collect => k2Provider.genericLogic(forApproach)(onRequest)

          case p@math.systemJ.J6.PowBy =>  // on Power
            // must handle Power dataType. HERE WE CAN OPTIMIZED.
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.systemK.K1.Power, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case _ => ???
        }
      }
    }

    // ORDER MATTERS! Need newest first, then subsequent branches shouldn't matter
    monoidInstance.combine(k2j6Provider, monoidInstance.combine(j6Provider, k2Provider))
  }
}

object K2J6 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
      (j6Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
        k2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
        ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new K2J6[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(j6Provider,k2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
      (j6Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
        k2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
        ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import imperativeControl.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._
    import paradigm.syntax._
    val mkImpl = new K2J6[paradigm.type, AIP, Unit](paradigm)
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

    mkImpl(j6Provider,k2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, returnInIf, ite)
  }
}


