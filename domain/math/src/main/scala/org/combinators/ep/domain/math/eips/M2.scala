package org.combinators.ep.domain.math.eips

import org.combinators.ep.domain.math
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.{AnyParadigm, ToTargetLanguageType}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Strings}

object M2 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type],
       ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
    EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val ppProvider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override def logic
          (forApproach: AIP[paradigm.type])
          (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._
        import forApproach.canLookupTypeInMethod

        assert(onRequest.request.op == math.M2.PrettyP)

        val result = onRequest.tpeCase match {
          case litC@math.M0.Lit =>
            val att = litC.attributes.head
            for {
              ty <- forApproach.toTargetLanguageType(att.tpe)
              res <- asString(onRequest.attributes(att), ty)
            } yield res
          case other =>
            val lAtt = other.attributes.head
            val rAtt = other.attributes.tail.head
            val operator =
              onRequest.request.op match {
                case math.M0.Add => "+"
                case math.M1.Sub => "-"
                case _ => ???
              }
            for {
              left <- forApproach.dispatch(SendRequest(
                onRequest.attributes(lAtt),
                math.M2.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
              right <- forApproach.dispatch(SendRequest(
                onRequest.attributes(rAtt),
                math.M2.getModel.baseDataType,
                onRequest.request,
                Some(onRequest)
              ))
              res <- makeString(Seq(left, right), "(", operator, ")")
            } yield res
        }
        result
      }
    }
    monoidInstance.combine(M1(paradigm)(ffiArithmetic), ppProvider)
  }
}
