package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, RealArithmetic}

object M9 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m8Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiImper:Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val m9Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.M9.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- m8Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiRealArithmetic.enable()
        } yield ()
      }

      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = Set.empty

      def applicable
        (forApproach: AIP[paradigm.type], potentialRequest:PotentialRequest): Boolean = {
        (potentialRequest.op == math.M9.Height) &&
          Set(math.M0.Lit, math.M0.Add, math.M1.Sub,math.M3.Divd,math.M3.Mult,math.M3.Neg,math.I2.Power,math.M8.Inv).contains(potentialRequest.tpeCase)
      }

      /** Generic logic takes care of the structure-based cases, only Lit needs special handling. */
      override def genericLogic
        (forApproach: AIP[paradigm.type])
        (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import AnyParadigm.syntax._
        import methodBodyCapabilities._

        if (onRequest.request.op == math.M9.Height && onRequest.tpeCase != math.M0.Lit) {
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
                    math.M4.getModel.baseDataType,
                    Request(math.M9.Height, Map.empty)
                  )
                )
                declVar <- ffiImper.imperativeCapabilities.declareVar(attName, intType, Some(exprVal))
                ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(maxDecl, declVar)

                ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
                  assignStmt <-  ffiImper.imperativeCapabilities.assignVar(maxDecl, declVar)
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
          m8Provider.genericLogic(forApproach)(onRequest)
        }
      }

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import paradigm._
        import AnyParadigm.syntax._
        import methodBodyCapabilities._

        assert(applicable(forApproach)(onRequest))

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
    monoidInstance.combine(m9Provider, m8Provider)
  }
}
