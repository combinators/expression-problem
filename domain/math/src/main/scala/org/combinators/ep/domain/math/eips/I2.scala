package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.{AnyParadigm, ToTargetLanguageType}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic, Strings}

object I2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val ppProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      def applicable
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = {
        (onRequest.request.op == math.I1.MultBy || onRequest.request.op == math.M0.Eval || onRequest.request.op == math.M2.PrettyP) &&
          (Set(math.I2.Power).contains(onRequest.tpeCase))
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {

        import paradigm._
        import methodBodyCapabilities._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiRealArithmetic.realArithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        assert(applicable(forApproach)(onRequest))

        val result = onRequest.tpeCase match {
          case power@math.I2.Power => {
            onRequest.request.op match {
              case eval@math.M0.Eval =>
                for {
                  base <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request,
                    Some(onRequest)
                  ))
                  exponent <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.tail.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request,
                    Some(onRequest)
                  ))
                  res <- pow(base, exponent)
                } yield res

              case pp@math.M2.PrettyP =>
                for {
                  base <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request,
                    Some(onRequest)
                  ))
                  exponent <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.tail.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request,
                    Some(onRequest)
                  ))

                  res <- makeString(Seq(base, exponent), "(", "^", ")")
                } yield res

                // NOTE: NO DISPATCHES in mult by....
              case mp@math.I1.MultBy =>
                for {

                  other <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.tail.head),
                    math.M1.getModel.baseDataType,
                    Request(math.M0.Eval, Map.empty),
                    Some(onRequest)
                   ))
                  baseEval <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.head),
                    math.M1.getModel.baseDataType,
                    Request(math.M0.Eval, Map.empty),
                    Some(onRequest)
                  ))
                  // lit(Math.log(this.convert(other).eval()) / Math.log(getBase().eval()))));

                  eulerNumFixMe <- forApproach.reify(InstanceRep(TypeRep.Double)(2.7182818284590452354))
                  numExpr <- log(eulerNumFixMe, other)
                  denomExpr <- log(eulerNumFixMe, baseEval)
                  fraction <- div(numExpr, denomExpr)
                  addend <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Lit, fraction)

                  expExpr <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M0.Add,
                    onRequest.attributes(power.attributes.tail.head), addend)

                  res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.I2.Power,
                    onRequest.attributes(power.attributes.head), expExpr)

                } yield res

              case _ => ???
            }
          }

          case _ => ???
        }
        result.map(Some(_))
      }
    }
    monoidInstance.combine(ppProvider, I1(paradigm)(ffiArithmetic, ffiRealArithmetic, ffiStrings, ffiImper))
  }
}
