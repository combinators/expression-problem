package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Booleans, Equality, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

sealed class I2M3I1N1[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]](val paradigm: P) {

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]],m3i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],n1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
    ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val i2m3i1n1_provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.I2M3I1N1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiStrings.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiEquality.enable()

          _ <- i2Provider.initialize(forApproach)
          _ <- m3i1Provider.initialize(forApproach)
          _ <- n1Provider.initialize(forApproach)
        } yield ()
      }

      //ONLY have to handle PowBy, Power
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.I2M3I1N1.getModel.flatten.typeCases
        (potentialRequest.op, potentialRequest.tpeCase) match {
          case (math.N1.PowBy, tpeCase) if cases.contains(tpeCase) => Some(Set.empty)

          // first chance to bring MultBy to bear on Power when Mult optimization is in effect. This is tricky!
          case (math.systemI.I1.MultBy, math.systemI.I2.Power)  => Some(Set.empty)

          // rest handled above by first two cases
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

          case p@math.N1.PowBy =>  // on Power
            // must handle Power dataType. HERE WE CAN OPTIMIZE.
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.systemI.I2.Power, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case mb@math.systemI.I1.MultBy => // WE CAN OPTIMIZE MultBy with Mult
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.M3.Mult, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)

          case _ => ???
        }
      }
    }

    // newest first
    monoidInstance.combine(i2m3i1n1_provider, monoidInstance.combine(monoidInstance.combine(i2Provider, m3i1Provider), n1Provider))
  }
}

object I2M3I1N1 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]],m3i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],n1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val mkImpl = new I2M3I1N1[paradigm.type, AIP](paradigm)

    mkImpl(i2Provider,m3i1Provider,n1Provider)(ffiBoolean, ffiEquality, ffiStrings)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]],m3i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],n1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val mkImpl = new I2M3I1N1[paradigm.type, AIP](paradigm)

    mkImpl(i2Provider,m3i1Provider,n1Provider)(ffiBoolean, ffiEquality, ffiStrings)
  }
}