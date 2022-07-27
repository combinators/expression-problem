package org.combinators.ep.domain.math.eips   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.{abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

// Code for M8. Takes adapters for return in if-then-else, s.t. functional- and imperative-style if-then-else can be
// used in an uniform way.
sealed class V1[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply
  (c2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
   ifThenElse: IfThenElseCommand
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val v1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.V1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- c2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquality.enable()
        } yield ()
      }

      def applicable(forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        Set(math.C2.Collect,math.M2.PrettyP,math.M0.Eval,math.Q1.Identifier,Operation.asTree).contains(potentialRequest.op) &&
          Set(math.V1.Inv).contains(potentialRequest.tpeCase)
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        c2Provider.genericLogic(forApproach)(onRequest)

      def logic(forApproach: AIP[paradigm.type])
               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._

        onRequest.request.op match {
          case math.C2.Collect =>
            c2Provider.genericLogic(forApproach)(onRequest)
          case math.Q1.Identifier =>
            c2Provider.genericLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => c2Provider.genericLogic(forApproach)(onRequest)

          case math.M0.Eval =>
            onRequest.tpeCase match {
              case math.V1.Inv =>    // opposite of DivD
                for {
                  atts <- forEach (onRequest.tpeCase.attributes) { att =>
                    forApproach.dispatch(SendRequest(
                      onRequest.attributes(att),
                      math.M3.getModel.baseDataType,
                      onRequest.request
                    ))
                  }

                  res <- div(atts.tail.head, atts.head)   // SWAP order
                } yield Some(res)

              case _ => ???
            }

          case math.M2.PrettyP =>
            onRequest.tpeCase match {
              // HACK: DON'T KNOW HOW TO FIX REIFY ISSUE
//              case math.V1.Inv => for {
//                atts <- forEach (onRequest.tpeCase.attributes) { att =>
//                  forApproach.dispatch(SendRequest(
//                    onRequest.attributes(att),
//                    math.M3.getModel.baseDataType,
//                    onRequest.request,
//                    Some(onRequest)
//                  ))
//                }
//
//                // swap ordering
//                res <- makeString(Seq(atts.tail.head, atts.head), "(", "/", ")")
//              } yield Some(res)

              case _ => ???
            }
          case _ => ???
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(v1Provider, c2Provider)
  }
}


object V1 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (c2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
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

    mkImpl(c2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (c2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
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

    mkImpl(c2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality, returnInIf, ite)
  }
}
