package org.combinators.ep.domain.shape.eips

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.{GenericModel, shape}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans}

object S2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (s1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val s2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      val model:GenericModel = shape.S2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- s1Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
        } yield ()
      }

      def applicable
      (forApproach: AIP[paradigm.type], potentialRequest: PotentialRequest): Boolean = {
        (potentialRequest.op == shape.S2.Shrink) &&
          (Set(shape.S1.Union,shape.S0.Translate,shape.S0.Circle, shape.S0.Square).contains(potentialRequest.tpeCase))
      }

      override def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import methodBodyCapabilities._

        // no need to pass up to the chain since only Eval is known
        assert(applicable(forApproach)(onRequest))

        onRequest.tpeCase match {

          case square@shape.S0.Square =>
              for {
                varName <- freshName(forApproach.names.mangle("shrunkSide"))
                baseType <- toTargetLanguageType(TypeRep.Double)
                resultVar <- ffiImper.imperativeCapabilities.declareVar(varName, baseType, Some(onRequest.request.arguments.head._2))

                multExpr <- ffiArithmetic.arithmeticCapabilities.mult(resultVar, onRequest.attributes.head._2)   // side
                multStmt <- ffiImper.imperativeCapabilities.assignVar(resultVar, multExpr)
                _ <- addBlockDefinitions(Seq(multStmt))

                res <- forApproach.instantiate(shape.S2.getModel.baseDataType, shape.S0.Square, resultVar)

              } yield Some(res)

            case circle@shape.S0.Circle =>
              for {
                varName <- freshName(forApproach.names.mangle("shrunkRadius"))
                baseType <- toTargetLanguageType(TypeRep.Double)
                resultVar <- ffiImper.imperativeCapabilities.declareVar(varName, baseType, Some(onRequest.request.arguments.head._2))

                multExpr <- ffiArithmetic.arithmeticCapabilities.mult(resultVar, onRequest.attributes.head._2)   // side
                multStmt <- ffiImper.imperativeCapabilities.assignVar(resultVar, multExpr)
                _ <- addBlockDefinitions(Seq(multStmt))

                res <- forApproach.instantiate(shape.S2.getModel.baseDataType, shape.S0.Circle, resultVar)

              } yield Some(res)

            /** Union dispatches to each individually */
            case union@shape.S1.Union =>
              for {
                s1Result <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(shape.S1.s1),
                  shape.S0.getModel.baseDataType,
                  Request(shape.S2.Shrink, Map(shape.S2.pct -> onRequest.request.arguments(shape.S2.pct)))
                ))

                s2Result <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(shape.S1.s2),
                  shape.S0.getModel.baseDataType,
                  Request(shape.S2.Shrink, Map(shape.S2.pct -> onRequest.request.arguments(shape.S2.pct)))
                ))

                res <- forApproach.instantiate(shape.S0.getModel.baseDataType, shape.S1.Union, s1Result, s2Result)
              } yield Some(res)

            // Seq(transx, transy, shape))
            case translate@shape.S0.Translate =>
              for {
                shapeResult <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(shape.S0.shape),
                  shape.S0.getModel.baseDataType,
                  Request(shape.S2.Shrink, Map(shape.S2.pct -> onRequest.request.arguments(shape.S2.pct)))
                ))

                res <- forApproach.instantiate(shape.S0.getModel.baseDataType, shape.S0.Translate,
                  onRequest.attributes.head._2, onRequest.attributes.tail.head._2, shapeResult)
              } yield Some(res)

          // Scala response to ultimately cause runtime exception
            case _ => ???

        }
      }
    }

    monoidInstance.combine(s2Provider, s1Provider)
  }
}