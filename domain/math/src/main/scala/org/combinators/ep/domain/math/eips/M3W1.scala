package org.combinators.ep.domain.math.eips     /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control.{Functional, Imperative}
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans, Equality, Strings}
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.domain.{GenericModel, abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

/** Upon merging M3 and W1 there is a need for MultByx(Divd, Mult, Neg) as well as a need for
 * (Collect,Simplify,Id,AsTree,Equals,PowBy)xPower
 *
 * These all have to be captured here...
 */
sealed class M3W1[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P], IfBlockType](val paradigm: P) {

  type IfThenElseCommand =
    (paradigm.syntax.Expression,
      Generator[paradigm.MethodBodyContext, IfBlockType],
      Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
      Generator[paradigm.MethodBodyContext, IfBlockType]) =>
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
    (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],w1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
    (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
     ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
     ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):

  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m3w1Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.M3W1.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiBoolean.enable()
          _ <- ffiEquality.enable()

          _ <- m3Provider.initialize(forApproach)
          _ <- w1Provider.initialize(forApproach)
        } yield ()
      }

      // PrettyP came from M2, so need to add for Power
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if (potentialRequest.op == math.M2.PrettyP && potentialRequest.tpeCase == math.W1.Power) {
          Some(Set.empty)
        } else {
          None
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {

        import paradigm._
        import methodBodyCapabilities._
        import ffiStrings.stringCapabilities._
        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        val result = onRequest.tpeCase match {
          case power@math.W1.Power => {
            onRequest.request.op match {

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

              case _ => ???
            }
          }
          case _ => ???
        }
        result.map(Some(_))
      }
    }

    // ORDER MATTERS! Need newest first, then subsequent branches shouldn't matter
    monoidInstance.combine(m3w1Provider, monoidInstance.combine(m3Provider, w1Provider))
  }
}


object M3W1 {
  def functional[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   w1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (functionalControl: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    val mkImpl = new M3W1[paradigm.type, AIP, Expression](paradigm)
    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          res <- functionalControl.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)

    mkImpl(m3Provider,w1Provider)(ffiArithmetic, ffiBoolean, ffiEquality, ffiStrings) // , expGen => expGen, ite)
  }

  def imperative[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m3Provider: EvolutionImplementationProvider[AIP[paradigm.type]],
   w1Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (imperativeControl: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    import paradigm.syntax._
    import paradigm.methodBodyCapabilities._
    import imperativeControl.imperativeCapabilities._
    val mkImpl = new M3W1[paradigm.type, AIP, Unit](paradigm)
    val returnInIf: Generator[paradigm.MethodBodyContext, Expression] => Generator[paradigm.MethodBodyContext, Unit] =
      expGen =>
        for {
          resultExp <- expGen
          resultStmt <- returnStmt(resultExp)
          _ <- addBlockDefinitions(Seq(resultStmt))
        } yield None

    val ite: mkImpl.IfThenElseCommand =
      (cond, ifBlock, ifElseBlocks, elseBlock) =>
        for {
          resultStmt <- ifThenElse(cond, ifBlock, ifElseBlocks, Some(elseBlock))
          _ <- addBlockDefinitions(Seq(resultStmt))
        } yield None

    mkImpl(m3Provider,w1Provider)(ffiArithmetic, ffiBoolean, ffiEquality, ffiStrings) // , returnInIf, ite)
  }
}


