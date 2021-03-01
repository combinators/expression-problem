package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}

// Code for M8. Takes adapters for return in if-then-else, s.t. functional- and imperative-style if-then-else can be
// used in an uniform way.
sealed class M8[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply
  (m7i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
   ifThenElse: IfThenElseCommand
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m8Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M8.getModel

      /** Simplify depends upon having a working eval. */
      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
          op match {
            case math.M4.Simplify => Set(math.M0.Eval)
            case _ => Set.empty
          }
      }

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m7i2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquality.enable()
        } yield ()
      }

      def applicable(forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        Set(math.M4.Simplify,math.M4.Collect,math.M2.PrettyP,math.M0.Eval,math.I1.MultBy,math.M7.PowBy,math.M6.Equals,math.M5.Identifier,Operation.asTree).contains(potentialRequest.op) &&
          Set(math.M8.Inv).contains(potentialRequest.tpeCase)
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
            case math.M8.Inv =>
              vals.flatMap { case List(leftVal, rightVal) =>
                for {
                  minusOne <- forApproach.reify(InstanceRep(TypeRep.Double)(-1.0))

                  rightEqZero <- areEqual(doubleTy, rightVal, zero)
                  leftEqOne <- areEqual(doubleTy, leftVal, one)
                  leftRightEq <- areEqual(doubleTy, leftVal, rightVal)
                  negLeftVal <- mult(minusOne, leftVal)
                  rightLeftNeqEq <- areEqual(doubleTy, rightVal, negLeftVal)

                  result <-
                    ifThenElse(
                      rightEqZero, returnInIf(zeroLit),
                      Seq(
                        (leftEqOne, returnInIf(simplifyRec(atts.tail.head, attExprs.tail.head))),
                        (leftRightEq, returnInIf(oneLit)),
                        (rightLeftNeqEq, returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, minusOne)))
                      ),
                      for {
                        lsimp <- simplifyRec(atts.head, attExprs.head)
                        rsimp <- simplifyRec(atts.tail.head, attExprs.tail.head)
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.M8.Inv, lsimp, rsimp))
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
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        m7i2Provider.genericLogic(forApproach)(onRequest)


      def logic(forApproach: AIP[paradigm.type])
               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)
        import ffiStrings.stringCapabilities._
        import ffiArithmetic.arithmeticCapabilities._
        import paradigm._
        import methodBodyCapabilities._      // Don't be fooled. NEEDS THIS ONE
        import AnyParadigm.syntax._


        val atts = for {
          atts <- forEach (onRequest.tpeCase.attributes) { att =>
            forApproach.dispatch(SendRequest(
              onRequest.attributes(att),
              math.M3.getModel.baseDataType,
              onRequest.request,
              Some(onRequest)
            ))
          }
        } yield atts

        onRequest.request.op match {
          case math.M4.Collect =>
            m7i2Provider.genericLogic(forApproach)(onRequest)
          case math.M4.Simplify => simplifyLogic(forApproach)(onRequest)
          case math.I1.MultBy => m7i2Provider.genericLogic(forApproach)(onRequest)
          case math.M7.PowBy => m7i2Provider.genericLogic(forApproach)(onRequest)
          case math.M6.Equals => m7i2Provider.genericLogic(forApproach)(onRequest)
          case math.M5.Identifier =>
            m7i2Provider.genericLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => m7i2Provider.genericLogic(forApproach)(onRequest)

          case math.M0.Eval =>
            onRequest.tpeCase match {
              case math.M8.Inv =>
                for {
                  atts <- forEach (onRequest.tpeCase.attributes) { att =>
                    forApproach.dispatch(SendRequest(
                      onRequest.attributes(att),
                      math.M3.getModel.baseDataType,
                      onRequest.request,
                      Some(onRequest)
                    ))
                  }

                  res <- div(atts.tail.head, atts.head)   // SWAP order
                } yield Some(res)

              case _ => ???
            }

          case math.M2.PrettyP =>
            onRequest.tpeCase match {
              case math.M8.Inv => for {
                atts <- forEach (onRequest.tpeCase.attributes) { att =>
                  forApproach.dispatch(SendRequest(
                    onRequest.attributes(att),
                    math.M3.getModel.baseDataType,
                    onRequest.request,
                    Some(onRequest)
                  ))
                }

                // swap ordering
                res <- makeString(Seq(atts.tail.head, atts.head), "(", "/", ")")
              } yield Some(res)

              case _ => ???
            }
          case _ => ???
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(m8Provider, m7i2Provider)
  }
}

object M8 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m7i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new M8[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(m7i2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m7i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    import paradigm.methodBodyCapabilities._
    import imperativeControl.imperativeCapabilities._
    val mkImpl = new M8[paradigm.type, AIP, Unit](paradigm)
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

    mkImpl(m7i2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, returnInIf, ite)
  }
}
