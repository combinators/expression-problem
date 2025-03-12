package org.combinators.ep.domain.math.eips      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{abstractions, math}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.control.{ConstructorPattern, Imperative}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Functional, control}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Equality, RealArithmetic, Strings}

object M9funct {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (m8Provider : EvolutionImplementationProvider[AIP[paradigm.type]])
  (functional:Functional.WithBase[paradigm.type],
   functionalControl: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiEquals: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ):
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
        import paradigm._
        import AnyParadigm.syntax._
        import methodBodyCapabilities._
        import functionalControl.functionalCapabilities._

//        def makeCtorPat(tpe: paradigm.syntax.Type, caseName: paradigm.syntax.Name, attributeNames: Seq[paradigm.syntax.Name]): Generator[functionalControl.PatternContext, paradigm.syntax.Expression] = {
//          import functionalControl.patternCapabilities._
//          applyConstructorPattern(ConstructorPattern(tpe, caseName), attributeNames.map(patternVariable))
//        }
//

        if (onRequest.request.op == math.M9.Height && onRequest.tpeCase != math.M0.Lit) {
          val allCase = math.M9.getModel.flatten.typeCases
          for {
            zero <- forApproach.reify(InstanceRep(TypeRep.Int)(0))
            one <- forApproach.reify(InstanceRep(TypeRep.Int)(1))
            intType <- toTargetLanguageType(TypeRep.Int)


            //            _ <- forEach(onRequest.attributes.toSeq) { case (att, expr) => {
            //              for {
            //                attName <- freshName(forApproach.names.mangle(att.name))
            //                exprVal <- forApproach.dispatch(
            //                  SendRequest(
            //                    expr,
            //                    math.M4.getModel.baseDataType,
            //                    Request(math.M9.Height, Map.empty)
            //                  )
            //                )
            //                declVar <- ffiImper.imperativeCapabilities.declareVar(attName, intType, Some(exprVal))
            //                ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(maxDecl, declVar)
            //
            //                ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
            //                  assignStmt <-  ffiImper.imperativeCapabilities.assignVar(maxDecl, declVar)
            //                  _ <- addBlockDefinitions(Seq(assignStmt))
            //                } yield (),
            //                  Seq.empty
            //                )

            // insert logic for all types:
            // can get all types and find those with just one recursive inner
            // attribute; and the height method is defined

            //
            //         def height_rec(exp:Exp)
            //            exp match {
            //              case Sub(left, right) => { leftHeight = height(left); rightHeight = height(right) 1 + if (leftHeight > rightHeight) leftHeight rightHeight }
            //              case Mult(left, right) => { 1 + max(height_rec(left), height_rec(right)) }
            //              case Lit(value) => { 0 }
            //              case Add(left, right) => { 1 + max(height_rec(left), height_rec(right)) }
            //              case Neg(inner) => { 1 + eval(inner) }
            //            }
            //
            //           def height(exp : mathdomain.Exp): Integer = {
            //              val leftHeight = height_rec(exp)
            //           }
            //
            //
            //            maxName <- freshName(forApproach.names.mangle("max"))
            //            maxDecl <- ffiImper.imperativeCapabilities.declareVar(maxName, intType, Some(zero))
            //
            //            _ <- forEach(onRequest.attributes.toSeq) { case (att, expr) => {
            //              for {
            //                attName <- freshName(forApproach.names.mangle(att.name))
            //                exprVal <- forApproach.dispatch(
            //                  SendRequest(
            //                    expr,
            //                    math.M4.getModel.baseDataType,
            //                    Request(math.M9.Height, Map.empty)
            //                  )
            //                )
            //                declVar <- ffiImper.imperativeCapabilities.declareVar(attName, intType, Some(exprVal))
            //                ifExpr <- ffiArithmetic.arithmeticCapabilities.lt(maxDecl, declVar)
            //
            //                ifStmt <- ffiImper.imperativeCapabilities.ifThenElse(ifExpr, for {
            //                  assignStmt <-  ffiImper.imperativeCapabilities.assignVar(maxDecl, declVar)
            //                  _ <- addBlockDefinitions(Seq(assignStmt))
            //                } yield (),
            //                  Seq.empty
            //                )
            //
            //                _ <- addBlockDefinitions(Seq(ifStmt))
          } yield Some(one)
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
    monoidInstance.combine(m9Provider, m8Provider)
  }
}
