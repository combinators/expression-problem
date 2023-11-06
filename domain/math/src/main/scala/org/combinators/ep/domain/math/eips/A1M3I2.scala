package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ffi.Strings

object A1M3I2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (a1m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   i2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type])
  :
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val a1m3i2Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.A1M3I2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiStrings.enable()
          _ <- i2Provider.initialize(forApproach)
          _ <- a1m3Provider.initialize(forApproach)
        } yield ()
      }

//      override def applicableIn
//        (forApproach:  AIP[paradigm.type])
//        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression],currentModel:GenericModel): Option[GenericModel] = {
//
//        val forwardTable:PartialFunction[(Operation,DataTypeCase),GenericModel] = {
//
//          case (_, math.I2.Power) => math.I2.getModel
//
//          // handles everything else
//          case _ => math.A1M3.getModel
//        }
//
//        val tblModel = forwardTable.lift(onRequest.request.op, onRequest.tpeCase)
//
//        // Because EIP could be "further in future" then a given model, we need to be sure to
//        // only return forwarding information when we have a hit on the currentModel.
//        if (model == currentModel || model.before(currentModel)) {
//          tblModel
//        } else {
//          None
//        }
//      }
//
//      // NOTHING NEW!
//      def applicable(forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = false

      // brings in Power but no new operations so there SHOULD be nothing to do
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        None
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

    // HACK HACK HACK EMPTY CAN REMOVE
        def operate(atts: Seq[syntax.Expression]): Generator[paradigm.MethodBodyContext, syntax.Expression] =
          onRequest.request.op match {
           case _ => ???
          }

        val result =
          for {
            atts <- forEach (onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.M3.getModel.baseDataType,
                onRequest.request
              ))
            }
            res <- operate(atts)
          } yield res

        result.map(Some(_))
      }
    }

    // ORDER IMPORTANT
    monoidInstance.combine(a1m3i2Provider, monoidInstance.combine(a1m3Provider, i2Provider))
  }
}
