package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{GenericModel, abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

// Code for M8. Takes adapters for return in if-then-else, s.t. functional- and imperative-style if-then-else can be
// used in an uniform way.
sealed class J7[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply
  (k2j6Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
   ifThenElse: IfThenElseCommand
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j7Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.J7.getModel


      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- k2j6Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquality.enable()
        } yield ()
      }

//      /** Simplify depends upon having a working eval. */
//      override def dependencies(op: Operation, dt: DataTypeCase): Option[Set[Operation]] = {
//        op match {
//          case math.K2.Simplify => Some(Set(math.M0.Eval))
//          // Since we are defining new type, we have to "carry over" the dependencies for Eql
//          case op if math.J2.isOps(model.flatten.typeCases).contains(op) => Some(Set(math.J2.Eql))
//          case _ => None
//        }
//      }
//      def applicable(forApproach: AIP[paradigm.type], potentialRequest: PotentialRequest): Boolean = {
//        potentialRequest.op.tags.contains(math.J2.IsOp) ||
//        //  Set(math.J1.MultBy, math.J6.PowBy).contains(potentialRequest.op) ||  // short-circuit all of these for Inv only
//          (Set(math.K2.Simplify, math.K2.Collect, math.J3.PrettyP, math.M0.Eval, math.J1.MultBy, math.J6.PowBy, math.J5.Equals, math.J2.Eql, math.J4.Identifier, Operation.asTree).contains(potentialRequest.op) &&
//            Set(math.J7.Inv).contains(potentialRequest.tpeCase))
//      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        // TODO: dependency fix
        None
      }
      

//      override def applicableIn(forApproach:  AIP[paradigm.type], onRequest: PotentialRequest,currentModel:GenericModel): Option[GenericModel] = {
//        // must be designed to only return (to be safe) Java-accessible which is former branch only one step in past.
//        val forwardTable: PartialFunction[(Operation, DataTypeCase), GenericModel] = {
//          case (op, tpe) if op.tags.contains(math.J2.IsOp) => math.J2.getModel // where isXXX is generically defined
//
//          case (math.K2.Collect, math.J3.Divd) => model // I have to handle this
//          case (math.K2.Collect, math.J3.Neg) => model // I have to handle this
//          case (math.K2.Collect, _) => math.K2.getModel
//
//          case (math.K2.Simplify, math.J3.Divd) => model // I have to handle this
//        }
//
//        val tblModel = forwardTable.lift(onRequest.op, onRequest.tpeCase)
//
//        // Because EIP could be "further in future" then a given model, we need to be sure to
//        // only return forwarding information when we have a hit on the currentModel.
//        if (model == currentModel || model.before(currentModel)) {
//          tblModel
//        } else {
//          None
//        }
//      }

      private def simplifyLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiEquality.equalityCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import syntax._

        def evalChildren(tpe: DataTypeCase, atts: Map[abstractions.Attribute, Expression]): Generator[MethodBodyContext, List[Expression]] =
          forEach(atts.keys.toSeq) { att: abstractions.Attribute => {
            val expr: Expression = atts.get(att).get
            forApproach.dispatch(
              SendRequest(
                expr,
                math.M0.getModel.baseDataType,
                Request(math.M0.Eval, Map.empty)
              )
            )
          }
          }

        def simplifyRec(att: abstractions.Attribute, attExpr: Expression): Generator[MethodBodyContext, Expression] = {
          forApproach.dispatch(
            SendRequest(
              attExpr,
              math.M0.getModel.baseDataType,
              Request(math.K2.Simplify, Map.empty)
            )
          )
        }

        def simplifyOp(
                        atts: Seq[abstractions.Attribute],
                        attExprs: Seq[Expression],
                        vals: Generator[MethodBodyContext, List[Expression]],
                        doubleTy: Type,
                        zero: Expression,
                        one: Expression,
                        zeroLit: Generator[MethodBodyContext, Expression],
                        oneLit: Generator[MethodBodyContext, Expression]
                      ): Generator[MethodBodyContext, Option[Expression]] = {
          onRequest.tpeCase match {
            case math.J7.Inv =>
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
                        res <- returnInIf(forApproach.instantiate(math.M0.getModel.baseDataType, math.J7.Inv, lsimp, rsimp))
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
        k2j6Provider.genericLogic(forApproach)(onRequest)


      def logic(forApproach: AIP[paradigm.type])
               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        //assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name) TODO: fix assert
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._ // needed for reify (in makeString)

        // need to know when isOp is not in the onRequest Type (to handle return false; default implementation)
        // because then we can return FALSE
       
        if (onRequest.request.op != math.J2.isOp(onRequest.tpeCase) && onRequest.request.op.tags.contains(math.J2.IsOp)) {
          import ffiBoolean.booleanCapabilities._
          for {
            booleanFalse <- falseExp
          } yield Some(booleanFalse)
        } else if (onRequest.request.op == math.J2.isOp(onRequest.tpeCase)) {
          genericLogic(forApproach)(onRequest) // same isOpTypeCase applied to TypeCase can pass in
        } else {
          // if opname is a "isSub" or "isAdd" for older typecase, but we are in newest one? Still send to generic logic
          val pastOps = math.J2.isOps(model.flatten.typeCases)
          if (pastOps.contains(onRequest.request.op)) {
            genericLogic(forApproach)(onRequest)
//          } else if (onRequest.request.op == math.J1.MultBy) {
//            /* Handle MultBy with these data types. */
//            onRequest.tpeCase match {
//              case math.J4.Power => genericLogic(forApproach)(onRequest)
//              case _ => ???
//            }
          } else if (onRequest.request.op == math.J2.Eql) {
            genericLogic(forApproach)(onRequest)
          } else {
            val atts = for {
              atts <- forEach(onRequest.tpeCase.attributes) { att =>
                forApproach.dispatch(SendRequest(
                  onRequest.attributes(att),
                  math.J3.getModel.baseDataType,
                  onRequest.request
                ))
              }
            } yield atts

            onRequest.request.op match {
              case op if op.tags.contains(math.J2.IsOp) => k2j6Provider.genericLogic(forApproach)(onRequest) // where isXXX is generically defined

              case math.K2.Collect =>
                k2j6Provider.genericLogic(forApproach)(onRequest)
              case math.K2.Simplify => simplifyLogic(forApproach)(onRequest)
              case math.J1.MultBy => // take advantage of Mult
                if (onRequest.tpeCase == math.J7.Inv) {
                  for {
                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.J2.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
                  } yield Some(res)
                } else {
                  k2j6Provider.logic(forApproach)(onRequest)
                }

              case math.J6.PowBy => // take advantage of new Power
                if (onRequest.tpeCase == math.J7.Inv) {
                  for {
                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.K1.Power, onRequest.selfReference, onRequest.request.arguments.head._2)
                  } yield Some(res)
                } else {
                  k2j6Provider.logic(forApproach)(onRequest)
                }

              case math.J5.Equals => k2j6Provider.genericLogic(forApproach)(onRequest)
              case math.J2.Eql => k2j6Provider.genericLogic(forApproach)(onRequest)
              case math.J4.Identifier =>
                k2j6Provider.genericLogic(forApproach)(onRequest)
              case op if op.tags.contains(math.J2.IsOp) => k2j6Provider.genericLogic(forApproach)(onRequest)

              case op if op == Operation.asTree => k2j6Provider.genericLogic(forApproach)(onRequest)

              case math.M0.Eval =>
                onRequest.tpeCase match {
                  case math.J7.Inv =>
                    for {
                      atts <- forEach(onRequest.tpeCase.attributes) { att =>
                        forApproach.dispatch(SendRequest(
                          onRequest.attributes(att),
                          math.M0.getModel.baseDataType,
                          onRequest.request
                        ))
                      }

                      res <- div(atts.tail.head, atts.head) // SWAP order
                    } yield Some(res)

                  case _ => ???
                }

              case math.J3.PrettyP =>
                onRequest.tpeCase match {
                  case math.J7.Inv => for {
                    atts <- forEach(onRequest.tpeCase.attributes) { att =>
                      forApproach.dispatch(SendRequest(
                        onRequest.attributes(att),
                        math.M0.getModel.baseDataType,
                        onRequest.request
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
      }
    }

    // newest one must come first
    monoidInstance.combine(j7Provider, k2j6Provider)
  }
}



object J7 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (k2j6Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new J7[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(k2j6Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (k2j6Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    import paradigm.methodBodyCapabilities._
    import imperativeControl.imperativeCapabilities._
    val mkImpl = new J7[paradigm.type, AIP, Unit](paradigm)
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

    mkImpl(k2j6Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, returnInIf, ite)
  }
}
