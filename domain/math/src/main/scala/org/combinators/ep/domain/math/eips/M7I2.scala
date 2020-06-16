package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{GenericModel, abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}

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

      /** Nothing special here */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        Set.empty
      }

      def applicable
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (Set(math.M7.PowBy,math.M6.Equals,Operation.asTree,math.M5.Identifier,math.M4.Collect,math.M4.Simplify).contains(onRequest.request.op) &&
          Set(math.I2.Power).contains(onRequest.tpeCase)) ||
          (Set(math.I1.MultBy).contains(onRequest.request.op) &&
            Set(math.M3.Divd, math.M3.Mult, math.M3.Neg).contains(onRequest.tpeCase))
      }

      override def applicableIn(forApproach:  AIP[paradigm.type])(onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression],currentModel:GenericModel): Option[GenericModel] = {
        // must be designed to only return (to be safe) Java-accessible which is former branch only one step in past.
        val forwardTable:PartialFunction[(Operation,DataTypeCase),GenericModel] = {
          case (math.I1.MultBy, math.M3.Divd) => model // I HANDLE these
          case (math.I1.MultBy, math.M3.Mult) => model // I HANDLE these
          case (math.I1.MultBy, math.M3.Neg) => model  // I HANDLE these
          case (math.I1.MultBy, _) => math.I1.getModel

          case (math.M2.PrettyP, math.I2.Power) => math.I2.getModel     // delegate
          case (math.M2.PrettyP, _) => math.M3.getModel

          case (math.M4.Collect, math.I2.Power) => model    // I have to handle this
          case (math.M4.Collect, _) => math.M4.getModel

          case (math.M4.Simplify, math.I2.Power) => model   // I have to handle this
          case (math.M4.Simplify, _) => math.M4.getModel

          case (math.M5.Identifier, math.I2.Power) => math.M5.getModel   // delegate to M7 branch for generic Logic
          case (math.M5.Identifier, _) => math.M5.getModel

          case (Operation.asTree, math.I2.Power) => model   // I have to handle this
          case (Operation.asTree, _) => math.M5.getModel

          case (math.M6.Equals, math.I2.Power) => model    // I have to handle this
          case (math.M6.Equals, _) => math.M6.getModel

          case (math.M7.PowBy, math.I2.Power) => model    // I have to handle this
          case (math.M7.PowBy, _) => math.M7.getModel
        }

        val tblModel = forwardTable.lift(onRequest.request.op, onRequest.tpeCase)

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
        import paradigm._
        import methodBodyCapabilities._
        import syntax._
        import AnyParadigm.syntax._
        import ffiEquality.equalityCapabilities._

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
            case math.I2.Power =>
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
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.I2.Power, lsimp, rsimp))
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

      def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._
        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)

        onRequest.request.op match {
          case math.M6.Equals => m7Provider.genericLogic(forApproach)(onRequest)
          case math.M4.Simplify => simplifyLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => m7Provider.genericLogic(forApproach)(onRequest)
          case math.M5.Identifier => m7Provider.genericLogic(forApproach)(onRequest)
          case math.M4.Collect => m7Provider.genericLogic(forApproach)(onRequest)

          case mb@math.I1.MultBy =>    // WE CAN OPTIMIZE MultBy with Mult
             for {
               res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
             } yield Some(res)

          case p@math.M7.PowBy =>  // on Power
          // must handle Power dataType. HERE WE CAN OPTIMIZED.
          val atts = onRequest.attributes.keys.toSeq
            val attExprs = onRequest.attributes.values.toSeq
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.I2.Power, onRequest.selfReference, onRequest.request.arguments.head._2)
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

    mkImpl(m7Provider,i2Provider)(ffiArithmetic, ffiBoolean, ffiEquality, returnInIf, ite)
  }
}
