package org.combinators.ep.domain.math.eips.systemJK      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control.Functional
import org.combinators.cogen.paradigm.ffi.Arithmetic
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.cogen.paradigm.control
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J8funct {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j7Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j8Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemJK.J8.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j7Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.M9.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          (potentialRequest.op, potentialRequest.tpeCase) match {
            case (math.systemJK.J8.Height, _) => Some(Set.empty)
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
            case binary@(math.systemJK.J7.Inv | math.systemJ.J2.Mult | math.systemJ.J3.Divd | math.M0.Add | math.systemJ.J1.Sub | math.systemK.K1.Power) =>
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
          j7Provider.genericLogic(forApproach)(onRequest)
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

          case neg@math.systemJ.J3.Neg =>
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
    monoidInstance.combine(j8Provider, j7Provider)
  }
}