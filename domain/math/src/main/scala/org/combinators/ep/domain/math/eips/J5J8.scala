package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{GenericModel, abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

/** Upon merging M7 and I2 there is a need for MultByx(Divd, Mult, Neg) as well as a need for
 * (Collect,Simplify,Id,AsTree,Equals,PowBy)xPower
 *
 * These all have to be captured here...
 */
sealed class J5J8[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (j8Provider: EvolutionImplementationProvider[AIP[paradigm.type]],j5Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
     returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
     ifThenElse: IfThenElseCommand):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j5j8Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.J5J8.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiEquality.enable()

          _ <- j8Provider.initialize(forApproach)
          _ <- j5Provider.initialize(forApproach)
        } yield ()
      }

      /** Eql dependencies propagate. */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        Set.empty
      }

      // TODO: Why isn't PrettyP in this applicable check, since it appears below in the applicableIn. Because it was there before the branching,
      // TODO: and so datatypes know what to do.
      def applicable
      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        potentialRequest.op.tags.contains(math.J2.IsOp) || (
        (Set(math.J3.PrettyP,math.J6.Identifier,Operation.asTree,math.J7.Equals,math.J8.PowBy).contains(potentialRequest.op) &&
          Set(math.J4.Power).contains(potentialRequest.tpeCase)) ||
          (Set(math.J5.Simplify,math.J5.Collect).contains(potentialRequest.op) &&
            Set(math.J3.Divd, math.J3.Neg).contains(potentialRequest.tpeCase)))
      }

      override def applicableIn(forApproach:  AIP[paradigm.type], onRequest: PotentialRequest,currentModel:GenericModel): Option[GenericModel] = {
        // must be designed to only return (to be safe) Java-accessible which is former branch only one step in past.
        val forwardTable:PartialFunction[(Operation,DataTypeCase),GenericModel] = {
          case (op,tpe) if op.tags.contains(math.J2.IsOp) => math.J2.getModel    // where isXXX is generically defined

          case (math.J3.PrettyP, math.J4.Power) => model   // I have to handle this
          case (math.J3.PrettyP, _) => math.J3.getModel

          case (math.J5.Collect, math.J3.Divd) => model    // I have to handle this
          case (math.J5.Collect, math.J3.Neg) => model    // I have to handle this
          case (math.J5.Collect, _) => math.J5.getModel

          case (math.J5.Simplify, math.J4.Power) => model   // I have to handle this
          case (math.J5.Simplify, _) => math.J5.getModel

          case (math.J6.Identifier, math.J4.Power) => model   // I have to handle this (generically)
          case (math.J6.Identifier, _) => math.J6.getModel

          case (Operation.asTree, math.J4.Power) => model   // I have to handle this
          case (Operation.asTree, _) => math.J6.getModel

          case (math.J7.Equals, math.J4.Power) =>
            model    // I have to handle this
          case (math.J7.Equals, _) => math.J7.getModel

          case (math.J8.PowBy, math.M0.Lit) => math.J8.getModel    // not sure why but perhaps it is a non-recursive type
          case (math.J8.PowBy, _) => model                  // I take responsibility
        }

        val tblModel = forwardTable.lift(onRequest.op, onRequest.tpeCase)

        // Because EIP could be "further in future" then a given model, we need to be sure to
        // only return forwarding information when we have a hit on the currentModel.
        if (model == currentModel || model.before(currentModel)) {
            tblModel
        } else {
          None
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
          forEach (atts.keys.toSeq) { att:abstractions.Attribute => {
            val expr:Expression = atts.get(att).get
            forApproach.dispatch(
              SendRequest(
                expr,
                math.M0.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty),
                Some(onRequest)
              )
            )}
          }

        def simplifyRec(att:abstractions.Attribute, attExpr: Expression): Generator[MethodBodyContext, Expression] = {
          forApproach.dispatch(
            SendRequest(
              attExpr,
              math.M0.getModel.baseDataType,
              Request(math.J5.Simplify, Map.empty),
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
          import ffiArithmetic.arithmeticCapabilities._
          onRequest.tpeCase match {
            case math.J3.Divd =>
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
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.J3.Divd, lsimp, rsimp))
                      } yield res
                    )
                } yield result
              }
            case math.J3.Neg =>
              vals.flatMap { case List(innerVal) =>
                for {
                  innerZero <- areEqual(doubleTy, innerVal, zero)
                  result <-
                    ifThenElse(
                      innerZero, returnInIf(zeroLit),
                      Seq.empty,
                      for {
                        innerSimp <- simplifyRec(atts.head, attExprs.head)
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.J3.Neg, innerSimp))
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
          j8Provider.genericLogic(forApproach)(onRequest)
        } catch {
          case _:RuntimeException | _:NotImplementedError => j5Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)

        import AnyParadigm.syntax._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._     // needed for reify (in makeString)
        onRequest.request.op match {
          case math.J7.Equals =>
            j8Provider.genericLogic(forApproach)(onRequest)
          case op if op == math.J2.isOp(onRequest.tpeCase) => j8Provider.genericLogic(forApproach)(onRequest)
          case op if op != math.J2.isOp(onRequest.tpeCase) && op.tags.contains(math.J2.IsOp) => j8Provider.genericLogic(forApproach)(onRequest)

          case math.J3.PrettyP =>
            onRequest.tpeCase match {
              case math.J4.Power =>
                for {
                    atts <- forEach(onRequest.tpeCase.attributes) { att =>
                      forApproach.dispatch(SendRequest(
                        onRequest.attributes(att),
                        math.J3.getModel.baseDataType,
                        onRequest.request,
                        Some(onRequest)
                      ))
                    }
                    res <- makeString(atts, "(", "^", ")")
                  } yield Some(res)
            }
          case math.J5.Simplify => simplifyLogic(forApproach)(onRequest)
          case op if op == Operation.asTree =>
            j8Provider.genericLogic(forApproach)(onRequest)
          case math.J6.Identifier =>
            j8Provider.genericLogic(forApproach)(onRequest)
          case math.J5.Collect => j5Provider.genericLogic(forApproach)(onRequest)

          case p@math.J8.PowBy =>  // on Power
          // must handle Power dataType. HERE WE CAN OPTIMIZED.
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.J4.Power, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case _ => ???
        }
      }
    }

    // ORDER MATTERS! Need newest first, then subsequent branches shouldn't matter
    monoidInstance.combine(j5j8Provider, monoidInstance.combine(j8Provider, j5Provider))
  }
}

object J5J8 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j8Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   j5Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new J5J8[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(j8Provider,j5Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j8Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   j5Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    import paradigm.methodBodyCapabilities._
    import imperativeControl.imperativeCapabilities._
    val mkImpl = new J5J8[paradigm.type, AIP, Unit](paradigm)
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

    mkImpl(j8Provider,j5Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, returnInIf, ite)
  }
}


