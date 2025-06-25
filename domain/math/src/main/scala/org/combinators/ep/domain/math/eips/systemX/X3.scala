package org.combinators.ep.domain.math.eips.systemX     /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.math
import org.combinators.ep.domain.math.systemX
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object X3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (x1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val x3Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.systemX.X3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- x1Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      // X3: ", Seq(Divd), Seq.empty)
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (Set(math.M0.Eval, math.systemX.X1.PrettyP, math.systemX.X1.MultBy).contains(potentialRequest.op) && Set(systemX.X3.Divd).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        def operate(attGenerators: Seq[Generator[paradigm.MethodBodyContext, syntax.Expression]]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case systemX.X3.Divd =>
                  for {
                    atts <- forEach(attGenerators)(g => g)
                    result <- div(atts*)
                  } yield result

                case _ => ???
              }

            case math.systemX.X1.MultBy =>
              onRequest.tpeCase match {
                case other@systemX.X3.Divd =>
                  val lAtt = other.attributes.head
                  val rAtt = other.attributes.tail.head

                  for {
                    left <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(lAtt),
                      math.M0.getModel.baseDataType,
                      onRequest.request
                    ))
                    right <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(rAtt),
                      math.M0.getModel.baseDataType,
                      onRequest.request
                    ))

                    res <- forApproach.instantiate(math.M0.getModel.baseDataType, other, left, right)
                  } yield res

                case _ => ???
              }

            case math.systemX.X1.PrettyP =>
              onRequest.tpeCase match {
                case systemX.X3.Divd =>
                  for {
                    atts <- forEach(attGenerators)(g => g)
                    result <- makeString(atts, "(", "/", ")")
                  } yield result

                case _ => ???
              }
            case _ => ???
          }

        val attGenerators = onRequest.tpeCase.attributes.map { att =>
          forApproach.dispatch(SendRequest(
            onRequest.attributes(att),
            math.M0.getModel.baseDataType,
            onRequest.request
          ))
        }

        val result =
          for {
            res <- operate(attGenerators)
          } yield res

        result.map(Some(_))
      }
    }

    // newest one must come first
    monoidInstance.combine(x3Provider, x1Provider)
  }
}
