package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Booleans, Equality, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

sealed class M3I1[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]](val paradigm: P) {

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
    ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m3i1_provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.M3I1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiStrings.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiEquality.enable()

          _ <- m3Provider.initialize(forApproach)
          _ <- i1Provider.initialize(forApproach)
        } yield ()
      }

      // MultBy with Mult but must also handle DivD and Neg
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.M3I1.getModel.flatten.typeCases
        (potentialRequest.op, potentialRequest.tpeCase) match {
          case (math.systemI.I1.MultBy, tpeCase) if cases.contains(tpeCase) => Some(Set.empty)

          case _ => None
        }
      }

      // NEED this since I have stated I will handle some of these
      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import paradigm._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {

          case mb@math.systemI.I1.MultBy => // WE CAN OPTIMIZE MultBy with Mult
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case _ => ???
        }
//        def operate(): Generator[paradigm.MethodBodyContext, syntax.Expression] =
//          onRequest.request.op match {
//            case mb@math.systemI.I1.MultBy =>      // take advantage of Mult data type
//              for {
//                res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
//              } yield res
//
//            case _ => ???
//          }
//
//        val result =
//          for {
//            atts <- forEach (onRequest.tpeCase.attributes) { att =>
//              forApproach.dispatch(SendRequest(
//                onRequest.attributes(att),
//                math.M3.getModel.baseDataType,
//                onRequest.request
//              ))
//            }
//            res <- operate()
//          } yield res
//
//        result.map(Some(_))
      }
    }

    // newest first
    monoidInstance.combine(m3i1_provider, monoidInstance.combine(i1Provider, m3Provider))
  }
}

object M3I1 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new M3I1[paradigm.type, AIP](paradigm)

    mkImpl(m3Provider,i1Provider)(ffiBoolean, ffiEquality, ffiStrings)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val mkImpl = new M3I1[paradigm.type, AIP](paradigm)

    mkImpl(m3Provider,i1Provider)(ffiBoolean, ffiEquality, ffiStrings)
  }
}
