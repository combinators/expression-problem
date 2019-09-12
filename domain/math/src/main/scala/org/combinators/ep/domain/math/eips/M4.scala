package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Lists, Strings}

object M4 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (paradigm: P)
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiLists: Lists.WithBase[paradigm.MethodBodyContext, paradigm.type]
    ):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val simplProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiLists.enable()
        } yield ()
      }

      def logic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        import ffiLists.listCapabilities._
        import ffiArithmetic.arithmeticCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import syntax._
        import AnyParadigm.syntax._

        def collectOp(listDoubleTy: Type): Generator[MethodBodyContext, Expression] = {
          val atts = onRequest.tpeCase.attributes.map(onRequest.attributes)
          onRequest.tpeCase match {
            case math.M0.Lit => create(listDoubleTy, atts)
            case _ =>
              for {
                collectedResults <- forEach (atts) { att =>
                    forApproach.dispatch(
                      SendRequest(
                        att,
                        math.M4.getModel.baseDataType,
                        Request(math.M4.Collect, Map.empty),
                        Some(onRequest)
                      )
                    )
                  }
                res <- collectedResults.foldLeft(create(listDoubleTy, Seq.empty)) { case (lastRes, nextCol) =>
                  for {
                    lastExp <- lastRes
                    nextExp <- nextCol
                    nextRes <- append(lastExp, nextExp)
                  } yield nextRes
                }
              } yield res
          }
        }

        def simplifyOp(listDoubleTy: Type): Generator[MethodBodyContext, Expression] = {
          val atts = onRequest.tpeCase.attributes.map(onRequest.attributes)
          onRequest.tpeCase match {
            case math.M0.Lit => Command.lift(onRequest.selfReference)
            case math.M0.Add =>


          }
        }



        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.M3.Divd => div(atts: _*)
                case math.M3.Mult => mult(atts: _*)
                case math.M3.Neg =>
                  for {
                    minusOne <- reify(TypeRep.Double, -1)
                    res <- mult(minusOne, atts.head)
                  } yield res
                case _ => ???
              }
            case math.M2.PrettyP =>
              onRequest.tpeCase match {
                case math.M3.Divd => makeString(atts, "(", "/", ")")
                case math.M3.Mult => makeString(atts, "(", "*", ")")
                case math.M3.Neg =>
                  for {
                    minus <- reify(TypeRep.String, "-")
                    res <- stringAppend(minus, atts.head)
                  } yield res
                case _ => ???
              }
            case _ => ???
          }

        val result =
          for {
            atts <- forEach (onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M2.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
            }
            res <- operate(atts)
          } yield res
        result
      }
    }
    monoidInstance.combine(M2(paradigm)(ffiArithmetic, ffiStrings), simplProvider)
  }
}
