package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control.{Functional, Imperative}
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}

/** Upon merging M7 and I2 there is a need for MultByx(Divd, Mult, Neg) as well as a need for
  * (Collect,Simplify,Id,AsTree,Equals,PowBy)xPower
  *
  * These all have to be captured here...
  */
sealed class M7I2[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (m7Provider: EvolutionImplementationProvider[AIP[paradigm.type]],i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
        ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
        returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
        ifThenElse: IfThenElseCommand):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m7i2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M7I2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiEquality.enable()

          _ <- m7Provider.initialize(forApproach)
          _ <- i2Provider.initialize(forApproach)
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.M7I2.getModel.flatten.typeCases
        (potentialRequest.op, potentialRequest.tpeCase) match {
          case (math.systemI.I1.MultBy, tpeCase) if cases.contains(tpeCase) => Some(Set.empty)
          case (math.M7.PowBy, tpeCase) if cases.contains(tpeCase) => Some(Set.empty)
          case (math.M4.Collect, math.systemI.I2.Power) => Some(Set.empty)
          case (math.M4.Simplify, math.systemI.I2.Power) => Some(Set(math.M0.Eval))
          case (math.M5.Identifier, math.systemI.I2.Power) => Some(Set.empty)
          case (Operation.asTree, math.systemI.I2.Power) => Some(Set(math.M5.Identifier))
          case (math.M6.Equals, math.systemI.I2.Power) => Some(Set(Operation.asTree))
          case (math.M6.Eql, math.systemI.I2.Power) => Some(Set(math.M6.isOp(math.systemI.I2.Power)))
          //case isOp if math.M6.isOps(cases).contains(isOp) => Some(if (isOp == math.M6.isOp(potentialRequest.tpeCase)) Set(math.M6.Eql) else Set.empty)

          // isPower => empty for any non power argument (returns false), eql for power argument (left and right eql)
          case (isOp, tpeCase) if isOp == math.M6.isOp(math.systemI.I2.Power) => Some(if (isOp == math.M6.isOp(tpeCase)) Set(math.M6.Eql) else Set.empty)
          // isXXX for power argument => empty, e.g. isAdd(power) = false
          case (isOp, math.systemI.I2.Power) if math.M6.isOps(cases).contains(isOp) => Some(Set.empty)
          // rest handled above by first two cases
          case (_, _) => None
        }
      }


      // Simplify of Power -- if exponent is 1, then ignore! If exponent is 0, turn to 1; if exponent is -1, turn to DivD
      private def simplifyLogic(forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        import syntax._
        import AnyParadigm.syntax._
        import ffiEquality.equalityCapabilities._

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
              Request(math.M4.Simplify, Map.empty)
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
            case math.systemI.I2.Power =>
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
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.systemI.I2.Power, lsimp, rsimp))
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
          m7Provider.genericLogic(forApproach)(onRequest)
        } catch {
          case _:RuntimeException | _:NotImplementedError => i2Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {
          case math.M6.Equals => m7Provider.genericLogic(forApproach)(onRequest)
          case math.M6.Eql => m7Provider.genericLogic(forApproach)(onRequest)
          case op if op == math.M6.isOp(onRequest.tpeCase) => m7Provider.genericLogic(forApproach)(onRequest)
          case op if op != math.M6.isOp(onRequest.tpeCase) && op.tags.contains(math.M6.IsOp) => m7Provider.genericLogic(forApproach)(onRequest)

          case math.M4.Simplify => simplifyLogic(forApproach)(onRequest)
          case op if op == Operation.asTree =>
            m7Provider.genericLogic(forApproach)(onRequest)
          case math.M5.Identifier =>
            m7Provider.genericLogic(forApproach)(onRequest)
          case math.M4.Collect => m7Provider.genericLogic(forApproach)(onRequest)

          case mb@math.systemI.I1.MultBy =>    // WE CAN OPTIMIZE MultBy with Mult
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case p@math.M7.PowBy =>  // on Power
            // must handle Power dataType. HERE WE CAN OPTIMIZED.
            val atts = onRequest.attributes.keys.toSeq
            val attExprs = onRequest.attributes.values.toSeq
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.systemI.I2.Power, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)
        }
      }
    }

    // ORDER MATTERS! Need newest first, then subsequent branches shouldn't matter
    monoidInstance.combine(m7i2Provider, monoidInstance.combine(m7Provider, i2Provider))
  }
}


object M7I2 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
      (m7Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
        i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
        ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new M7I2[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(m7Provider,i2Provider)(ffiArithmetic, ffiBoolean, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
      (m7Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
        i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
      (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
        ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    import paradigm.methodBodyCapabilities._
    import imperativeControl.imperativeCapabilities._
    val mkImpl = new M7I2[paradigm.type, AIP, Unit](paradigm)
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

    mkImpl(m7Provider,i2Provider)(ffiArithmetic, ffiBoolean, ffiEquality, returnInIf, ite)
  }
}
