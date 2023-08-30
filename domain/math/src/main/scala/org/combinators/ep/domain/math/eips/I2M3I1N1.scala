package org.combinators.ep.domain.math.eips    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.control.{Functional, Imperative}
import org.combinators.ep.generator.paradigm.ffi.{Booleans, Equality, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

sealed class I2M3I1N1[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]](val paradigm: P) {

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]],m3i1Provider: EvolutionImplementationProvider[AIP[paradigm.type]],n1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
    ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val i2m3i1n1_provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.I2M3I1N1.getModel

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

      def applicable
      (forApproach: AIP[paradigm.type], onRequest: PotentialRequest): Boolean = {
        Set(math.N1.PowBy).contains(onRequest.op) &&
          Set(math.I2.Power).contains(onRequest.tpeCase)
      }

      // NEED this since I have stated I will handle some of these
      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import paradigm._

        onRequest.request.op match {

          case p@math.M7.PowBy =>  // on Power
            // must handle Power dataType. HERE WE CAN OPTIMIZED.
            val atts = onRequest.attributes.keys.toSeq
            val attExprs = onRequest.attributes.values.toSeq
            for {
              res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.I2.Power, onRequest.selfReference, onRequest.request.arguments.head._2)
            } yield Some(res)
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
    import paradigm.syntax._
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