package org.combinators.ep.domain.math.eips   /*DD:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

sealed class V1[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply
  (c2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ): EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val v1Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.V1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- c2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiStrings.enable()
          _ <- ffiEquality.enable()
        } yield ()
      }

      // adds Inv data Type
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        (potentialRequest.op, potentialRequest.tpeCase) match {
          case (math.Q1.Identifier, math.V1.Inv) => Some(Set.empty)
          case (Operation.asTree, math.V1.Inv) => Some(Set(math.Q1.Identifier))
          case (math.M2.PrettyP, math.V1.Inv) => Some(Set.empty)
          case (math.C2.Collect, math.V1.Inv) => Some(Set.empty)
          case (math.M0.Eval, math.V1.Inv) => Some(Set.empty)

          case (_, _) => None
        }
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        c2Provider.genericLogic(forApproach)(onRequest)

      def logic(forApproach: AIP[paradigm.type])
               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._   // only eligible after import paradigm._ above

        onRequest.request.op match {
          case math.C2.Collect =>
            c2Provider.genericLogic(forApproach)(onRequest)
          case math.Q1.Identifier =>
            c2Provider.genericLogic(forApproach)(onRequest)
          case op if op == Operation.asTree => c2Provider.genericLogic(forApproach)(onRequest)

          case math.M0.Eval =>
            onRequest.tpeCase match {
              case math.V1.Inv =>    // opposite of DivD
                for {
                  atts <- forEach (onRequest.tpeCase.attributes) { att =>
                    forApproach.dispatch(SendRequest(
                      onRequest.attributes(att),
                      math.M3.getModel.baseDataType,
                      onRequest.request
                    ))
                  }

                  res <- div(atts.tail.head, atts.head)   // SWAP order
                } yield Some(res)

              case _ => ???
            }

          case math.M2.PrettyP =>
            onRequest.tpeCase match {
              case math.V1.Inv => for {
                atts <- forEach (onRequest.tpeCase.attributes) { att =>
                  forApproach.dispatch(SendRequest(
                    onRequest.attributes(att),
                    math.V1.getModel.baseDataType,
                    onRequest.request
                  ))
                }
                res <- makeString(atts.reverse, "(", "/", ")")
              } yield Some(res)

              case _ => ???
            }

          case _ => ???
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(v1Provider, c2Provider)
  }
}


object V1 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (c2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new V1[paradigm.type, AIP, Expression](paradigm)

    mkImpl(c2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (c2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val mkImpl = new V1[paradigm.type, AIP, Unit](paradigm)

    mkImpl(c2Provider)(ffiArithmetic, ffiBoolean, ffiStrings, ffiEquality)
  }
}
