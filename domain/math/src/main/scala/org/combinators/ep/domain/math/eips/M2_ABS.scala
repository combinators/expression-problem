package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.cogen.{Command, InstanceRep, TypeRep}
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

// Takes adapters for return in if-then-else, s.t. functional- and imperative-style if-then-else can be used in an uniform way.
sealed class M2_ABS[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {
  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply (m2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
       ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
       returnInIf: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] => Generator[paradigm.MethodBodyContext, IfBlockType],
       ifThenElse: IfThenElseCommand
      ):
    EvolutionImplementationProvider[AIP[paradigm.type]] = {

    val m2_abs_Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.M2_ABS.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- m2Provider.initialize(forApproach)
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val ops = math.M2_ABS.getModel.flatten.ops
        if (ops.contains(potentialRequest.op) && (potentialRequest.tpeCase == math.M2_ABS.Abs)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        m2Provider.genericLogic(forApproach)(onRequest)

      def logic
          (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        val result:Generator[MethodBodyContext, Option[paradigm.syntax.Expression]] = onRequest.tpeCase match {
          case abs@math.M2_ABS.Abs =>
            onRequest.request.op match {
              case pp@math.M2.PrettyP =>
                val innerAtt = abs.attributes.head

                // 'Abs(INNER)'
                for {
                  inner <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(innerAtt),
                    math.M2_ABS.getModel.baseDataType,
                    onRequest.request
                  ))

                  // Wrap inner-prettified with ABS(...)
                  res <- makeString(Seq(inner), "ABS(", "",")")
                } yield Some(res)

              case ev@math.M0.Eval =>
                for {
                  zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0))
                  negativeOne <- forApproach.reify(InstanceRep(TypeRep.Double)(-1.0))

                  innerVal <- forApproach.dispatch(
                    SendRequest(
                      onRequest.attributes.head._2,
                      math.M2_ABS.getModel.baseDataType,
                      Request(math.M0.Eval, Map.empty)
                    )
                  )

                  lessThanZero <- ffiArithmetic.arithmeticCapabilities.lt(innerVal, zero)
                  result <-
                    ifThenElse(
                      lessThanZero,
                      returnInIf(ffiArithmetic.arithmeticCapabilities.mult(negativeOne, innerVal)),
                      Seq.empty,
                      returnInIf(Command.lift(innerVal))
                    )
                } yield result

              case _ => ???
            }
          case _ => ???
        }
        result
      }
    }

    // newest one must come first
    monoidInstance.combine(m2_abs_Provider, m2Provider)
  }
}


object M2_ABS {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new M2_ABS[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(m2Provider)(ffiArithmetic, ffiStrings, expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (imperativeControl: control.Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    import paradigm.methodBodyCapabilities._
    import imperativeControl.imperativeCapabilities._
    val mkImpl = new M2_ABS[paradigm.type, AIP, Unit](paradigm)
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

    mkImpl(m2Provider)(ffiArithmetic, ffiStrings, returnInIf, ite)
  }
}

