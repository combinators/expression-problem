package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.{AnyParadigm, ToTargetLanguageType}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic, Strings}

object I2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val i2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.I2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- i1Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val ops = math.I2.getModel.flatten.ops
        if (potentialRequest.tpeCase == math.I2.Power && ops.contains(potentialRequest.op)) {
          potentialRequest.op match {
            case math.I1.MultBy => Some(Set(math.M0.Eval))
            case _ => Some(Set.empty)
          }
        } else {
          None
        }
      }

      def applicable
      (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        Set(math.I1.MultBy,math.M0.Eval,math.M2.PrettyP).contains(potentialRequest.op) &&
          Set(math.I2.Power).contains(potentialRequest.tpeCase)
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        i1Provider.genericLogic(forApproach)(onRequest)

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {

        import paradigm._
        import methodBodyCapabilities._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiRealArithmetic.realArithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        val result = onRequest.tpeCase match {
          case power@math.I2.Power => {
            onRequest.request.op match {
              case eval@math.M0.Eval =>
                for {
                  base <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request
                  ))
                  exponent <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.tail.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request
                  ))
                  res <- pow(base, exponent)
                } yield res

              case pp@math.M2.PrettyP =>
                for {
                  base <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request
                  ))
                  exponent <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.tail.head),
                    math.M1.getModel.baseDataType,
                    onRequest.request
                  ))

                  res <- makeString(Seq(base, exponent), "(", "^", ")")
                } yield res

                // NOTE: NO DISPATCHES in mult by....
              case mp@math.I1.MultBy =>
                for {

                  other <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.tail.head),
                    math.M1.getModel.baseDataType,
                    Request(math.M0.Eval, Map.empty)
                   ))
                  baseEval <- forApproach.dispatch(SendRequest(
                    onRequest.attributes(power.attributes.head),
                    math.M1.getModel.baseDataType,
                    Request(math.M0.Eval, Map.empty)
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

    // newest first
    monoidInstance.combine(i2Provider, i1Provider)
  }
}
