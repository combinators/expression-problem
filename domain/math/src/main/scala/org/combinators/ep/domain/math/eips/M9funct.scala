package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, Functional}
import org.combinators.cogen.paradigm.control.{ConstructorPattern, Imperative}
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{abstractions, math}
import org.combinators.cogen.Command.{Generator, lift}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.cogen.paradigm.control

object M9funct {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m8Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m9Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M9.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m8Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.M9.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          (potentialRequest.op, potentialRequest.tpeCase) match {
            case (math.M9.Height, _) => Some(Set.empty)
            case (_, _) => None
          }
        } else {
          None
        }
      }

      /** Generic logic takes care of the structure-based cases, only Lit needs special handling. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import functionalControl.functionalCapabilities._
        import ffiArithmetic.arithmeticCapabilities._

        if (onRequest.request.op == math.M9.Height) {
          onRequest.tpeCase match {

            // these are all binary cases
            case binary@(math.M3.Neg | math.M3.Mult | math.M3.Divd | math.M0.Add | math.M1.Sub | math.systemI.I2.Power | math.M8.Inv) =>
              for {
                left <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(binary.attributes.head),
                  math.M0.getModel.baseDataType,
                  onRequest.request
                ))

                right <- forApproach.dispatch(SendRequest(
                  onRequest.attributes(binary.attributes.tail.head),
                  math.M0.getModel.baseDataType,
                  onRequest.request
                ))

                ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(left, right)
                one <- forApproach.reify(InstanceRep(TypeRep.Int)(1))

                result <- ifThenElse(cond = ifExpr, ifBlock = add(one, right), elseIfs = Seq.empty, elseBlock = add(one, left))
              } yield Some(result)
          }
        } else {
          m8Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.tpeCase match {
          // Height of a Lit is ZERO.
          case math.M0.Lit =>
            for {
              zero <- forApproach.reify(InstanceRep(TypeRep.Int)(0))
            } yield Some(zero)

          case neg@math.M3.Neg =>
            for {
              one <- forApproach.reify(InstanceRep(TypeRep.Int)(1))

              inner <- forApproach.dispatch(SendRequest(
                onRequest.attributes(neg.attributes.head),
                math.M0.getModel.baseDataType,
                onRequest.request
              ))

              res <- add(one, inner)
            } yield Some(res)

          case _ => genericLogic(forApproach)(onRequest)
        }
      }
    }

    // newest first
    monoidInstance.combine(m9Provider, m8Provider)
  }
}
