package org.combinators.ep.domain.math.eips.systemJK    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, RealArithmetic}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemJK
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J8 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j7Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiImper: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j8Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemJK.J8.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j7Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
        } yield ()
      }

      // J8 adds Height operation
      // J6 adds  Seq.empty, Seq(PowBy))
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = systemJK.J8.getModel.flatten.typeCases
        if (cases.contains(potentialRequest.tpeCase)) {
          potentialRequest.op match {
            case systemJK.J8.Height => Some(Set.empty) // recursive dependency?
            case _ => None
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
        import AnyParadigm.syntax._
        import paradigm._
        import methodBodyCapabilities._

        if (onRequest.request.op == systemJK.J8.Height && onRequest.tpeCase != math.M0.Lit) {
          for {
            zero <- forApproach.reify(InstanceRep(TypeRep.Int)(0))
            one <- forApproach.reify(InstanceRep(TypeRep.Int)(1))
            intType <- toTargetLanguageType(TypeRep.Int)
            maxName <- freshName(forApproach.names.mangle("max"))
            maxDecl <- ffiImper.imperativeCapabilities.declareVar(maxName, intType, Some(zero))

            _ <- forEach(onRequest.attributes.toSeq) { case (att, expr) => {
              for {
                attName <- freshName(forApproach.names.mangle(att.name))
                exprVal <- forApproach.dispatch(
                  SendRequest(
                    expr,
                    math.M0.getModel.baseDataType,
                    Request(systemJK.J8.Height, Map.empty)
                  )
                )
                declVar <- ffiImper.imperativeCapabilities.declareVar(attName, intType, Some(exprVal))
                ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(maxDecl, declVar)

                ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
                  assignStmt <- ffiImper.imperativeCapabilities.assignVar(maxDecl, declVar)
                  _ <- addBlockDefinitions(Seq(assignStmt))
                } yield (),
                  Seq.empty
                )

                _ <- addBlockDefinitions(Seq(ifStmt))
              } yield ()
            }
            }

            resExpr <- ffiArithmetic.arithmeticCapabilities.add(maxDecl, one)
          } yield Some(resExpr)
        } else {
          j7Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.tpeCase match {
          case math.M0.Lit =>
            for {
              zero <- forApproach.reify(InstanceRep(TypeRep.Int)(0))
            } yield Some(zero)

          case _ => genericLogic(forApproach)(onRequest)

        }
      }
    }

    // newest first
    monoidInstance.combine(j8Provider, j7Provider)
  }
}
