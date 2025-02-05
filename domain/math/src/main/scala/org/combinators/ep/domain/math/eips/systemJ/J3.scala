package org.combinators.ep.domain.math.eips.systemJ   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Attribute, Operation, TypeRep}
import org.combinators.ep.domain.{GenericModel, math}
import org.combinators.ep.domain.math.systemJ
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Booleans, Strings}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object J3 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (j2Provider: EvolutionImplementationProvider[AIP[paradigm.type]])
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBoolean: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]):
  EvolutionImplementationProvider[AIP[paradigm.type]] = {
    val j3Provider: EvolutionImplementationProvider[AIP[paradigm.type]] = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model: GenericModel = math.systemJ.J3.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- j2Provider.initialize(forApproach)
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- ffiBoolean.enable()
        } yield ()
      }

      // Seq(Neg, Divd), Seq(PrettyP) ++ J2.isOps(Seq(Neg,Divd)))
      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        val cases = math.systemJ.J3.getModel.flatten.typeCases
        (potentialRequest.op, potentialRequest.tpeCase) match {

          // PrettyP has none
          case (math.systemJ.J3.PrettyP, _) => Some(Set.empty)

          // Eql has two additional typecases to work with
          case (math.systemJ.J2.Eql, math.systemJ.J3.Neg) => Some(Set(math.systemJ.J2.isOp(systemJ.J3.Neg)))
          case (math.systemJ.J2.Eql, math.systemJ.J3.Divd) => Some(Set(math.systemJ.J2.isOp(systemJ.J3.Divd)))

          // TODO: SHOULD this be advanced every subsequent evolution?? ExtensibleVisitor fails because in J3 it doesn't
          // generate necessary factory methods for 'makeIsLit' as well as nee
          // case (math.systemJ.J2.Eql, _) => Some(Set(math.systemJ.J2.isOp(potentialRequest.tpeCase)))

          case (isOp, tpeCase) if math.systemJ.J2.isOps(cases).contains(isOp) => Some(if (isOp == math.systemJ.J2.isOp(tpeCase)) Set(math.systemJ.J2.Eql) else Set.empty)

          // isXXX for inv argument => empty, e.g. isAdd(inv) = false
          case (isOp, systemJ.J3.Neg) if math.systemJ.J2.isOps(cases).contains(isOp) => Some(Set.empty)
          case (op, systemJ.J3.Neg) if systemJ.J3.getModel.flatten.ops.contains(op) => Some(Set.empty)
          case (isOp, systemJ.J3.Divd) if math.systemJ.J2.isOps(cases).contains(isOp) => Some(Set.empty)
          case (op, systemJ.J3.Divd) if systemJ.J3.getModel.flatten.ops.contains(op) => Some(Set.empty)

          case (_, _) => None
        }
      }

      /** Do not call 'assert' since might not be applicable. */
      override def genericLogic(forApproach: AIP[paradigm.type])
                               (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] =
        j2Provider.genericLogic(forApproach)(onRequest)

      def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import AnyParadigm.syntax._
        import ffiArithmetic.arithmeticCapabilities._
        import ffiStrings.stringCapabilities._
        import paradigm._
        import methodBodyCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        def operate(): Generator[paradigm.MethodBodyContext, Option[syntax.Expression]] = {

          val attsGen = for {
            atts <- forEach(onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                systemJ.J3.getModel.baseDataType,
                onRequest.request
              ))
            }
          } yield atts

          val realResult = onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case systemJ.J3.Divd => {
                  for {
                    atts <- attsGen
                    res <- div(atts: _*)
                  } yield res
                }
                case systemJ.J3.Neg =>
                  for {
                    atts <- attsGen
                    minusOne <- reify(TypeRep.Double, -1)
                    res <- mult(minusOne, atts.head)
                  } yield res
                case _ => ???
              }

            case systemJ.J3.PrettyP =>
              onRequest.tpeCase match {
                case litC@math.M0.Lit =>
                  val att = litC.attributes.head
                  for {
                    ty <- toTargetLanguageType(att.tpe)
                    res <- asString(onRequest.attributes(att), ty)
                  } yield res

                case math.M0.Add => {
                  for {
                    atts <- attsGen
                    res <- makeString(atts, "(", "+", ")")
                  } yield res
                }
                case math.systemJ.J1.Sub => {
                  for {
                    atts <- attsGen
                    res <- makeString(atts, "(", "-", ")")
                  } yield res
                }
                case math.systemJ.J2.Mult => {
                  for {
                    atts <- attsGen
                    res <- makeString(atts, "(", "*", ")")
                  } yield res
                }
                case systemJ.J3.Divd => {
                  for {
                    atts <- attsGen
                    res <- makeString(atts, "(", "/", ")")
                  } yield res
                }
                case systemJ.J3.Neg =>
                  for {
                    atts <- attsGen
                    minus <- reify(TypeRep.String, "-")
                    res <- stringAppend(minus, atts.head)
                  } yield res
                case _ => ???
              }
            case _ => ???
          }
          realResult.map(Some(_))
        }

        // need to know when isOp is not in the onRequest Type (to handle return false; default implementation)
        // because then we can return FALSE
        if (onRequest.request.op != math.systemJ.J2.isOp(onRequest.tpeCase) && onRequest.request.op.tags.contains(math.systemJ.J2.IsOp)) {
          import ffiBoolean.booleanCapabilities._
          for {
            booleanFalse <- falseExp
          } yield Some(booleanFalse)
        } else if (onRequest.request.op == math.systemJ.J2.isOp(onRequest.tpeCase)) {
          genericLogic(forApproach)(onRequest) // same isOpTypeCase applied to TypeCase can pass in
        } else {
          // if opname is a "isSub" or "isAdd" for older typecase, but we are in newest one? Still send to generic logic
          val pastOps = math.systemJ.J2.isOps(model.flatten.typeCases)
          if (pastOps.contains(onRequest.request.op)) {
            genericLogic(forApproach)(onRequest)
          } else if (onRequest.request.op == math.systemJ.J1.MultBy) {
            /* Handle MultBy with these data types. */
            onRequest.tpeCase match {
              case divd@systemJ.J3.Divd =>
                for {
                  // this.left.multby(other)
                  numerator <- forApproach.dispatch(
                    SendRequest(
                      onRequest.attributes.toSeq.head._2,
                      onRequest.onType,
                      onRequest.request, // multby
                    )
                  )

                  res <- forApproach.instantiate(math.M0.getModel.baseDataType, systemJ.J3.Divd, numerator, onRequest.attributes(Attribute.right(model)))
                } yield Some(res)

              case systemJ.J3.Neg => genericLogic(forApproach)(onRequest)
              case _ => ???
            }
          } else if (onRequest.request.op == math.systemJ.J2.Eql) {
            genericLogic(forApproach)(onRequest)
          } else {
            operate()
          }
        }
      }
    }

    // newest one must come first
    monoidInstance.combine(j3Provider, j2Provider)
  }
}
