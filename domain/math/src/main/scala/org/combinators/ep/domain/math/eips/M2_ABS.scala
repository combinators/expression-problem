package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object M2_ABS {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (m2Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
       ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
       ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
    EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m2_abs_Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M2_ABS.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      def applicable
        (forApproach: AIP[paradigm.type], onRequest: PotentialRequest): Boolean = {
        (onRequest.op == math.M2.PrettyP || onRequest.op == math.M0.Eval) &&
          (Set(math.M2_ABS.Abs).contains(onRequest.tpeCase))
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

        assert(applicable(forApproach)(onRequest))

        val result = onRequest.tpeCase match {
          case abs@math.M2_ABS.Abs =>
            onRequest.request.op match {
              case pp@math.M2.PrettyP =>
                val innerAtt = abs.attributes.head

                // 'Abs(INNER)'
                for {
                  inner <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(innerAtt),
                    math.M2_ABS.getModel.baseDataType,
                    onRequest.request,
                    Some(onRequest)
                  ))

                  // Wrap inner-prettified with ABS(...)
                  res <- makeString(Seq(inner), "ABS(", "",")")
                } yield res

              case ev@math.M0.Eval =>
                // x = eval()     DONE
                // if x < 0       DONE
                //    x = -x      DONE
                // return x
                for {
                  zero <- forApproach.reify(InstanceRep(TypeRep.Double)(0))
                  doubleType <- toTargetLanguageType(TypeRep.Double)
                  innerResult <- freshName(forApproach.names.mangle("x"))
                  innerVal <- forApproach.dispatch(
                    SendRequest(
                      onRequest.attributes.head._2,
                      math.M2_ABS.getModel.baseDataType,
                      Request(math.M0.Eval, Map.empty),
                      Some(onRequest)
                    )
                  )
                  innerDecl <- ffiImper.imperativeCapabilities.declareVar(innerResult, doubleType, Some(innerVal))

                  ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(innerDecl, zero)
                  ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
                    negated <- ffiArithmetic.arithmeticCapabilities.sub(zero, innerDecl)
                    assignStmt <- ffiImper.imperativeCapabilities.assignVar(innerDecl, negated)
                    _ <- addBlockDefinitions(Seq(assignStmt))
                  } yield (),
                    Seq.empty
                  )

                  _ <- addBlockDefinitions(Seq(ifStmt))
                } yield innerDecl

              case _ => ???
            }
        }
        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(m2_abs_Provider, m2Provider)
  }
}
