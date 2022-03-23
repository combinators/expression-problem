package org.combinators.ep.domain.math.eips   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, TypeRep}
import org.combinators.ep.domain.math
import org.combinators.ep.domain.math.J2.Eql
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.EvolutionImplementationProvider.monoidInstance
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
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
    val j3Provider = new EvolutionImplementationProvider[AIP[paradigm.type]] {
      override val model = math.J2.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiStrings.enable()
          _ <- ffiBoolean.enable()
        } yield ()
      }

      override def dependencies(op:Operation, dt:DataTypeCase) : Set[Operation] = {
        // cannot forget that isXXX operations are dependencies (Extensible Visitor identified this oversight)
        val initial = math.J2.isOps(Seq(math.J3.Neg, math.J3.Divd)).toSet

        val new_ones = op match {
          case math.J2.Eql => math.J2.isOps(model.flatten.typeCases).toSet
          case op if math.J2.isOps(Seq(dt)).contains(op) => Set(math.J2.Eql)
          case _ => Set.empty
        }

        initial ++ new_ones
      }

      def applicable
      (forApproach: AIP[paradigm.type], potentialRequest: PotentialRequest): Boolean = {
        (Set(math.M0.Eval,math.J1.MultBy,math.J2.Eql).contains(potentialRequest.op) && Set(math.J3.Divd, math.J3.Neg).contains(potentialRequest.tpeCase)) ||
          Set(math.J3.PrettyP).contains(potentialRequest.op) ||
          math.J2.isOps(Seq(math.J3.Neg, math.J3.Divd)).contains(potentialRequest.op) || // needed for isTypeCase for new type cases being applied to old types... [handles isNeg x sub]
          Set(math.J3.Divd, math.J3.Neg).contains(potentialRequest.tpeCase)   // needed for isSub x Neg (and others)
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

        assert(applicable(forApproach)(onRequest), onRequest.tpeCase.name + " failed for " + onRequest.request.op.name)

        def operate(): Generator[paradigm.MethodBodyContext, Option[syntax.Expression]] = {

          val attsGen = for {
            atts <- forEach(onRequest.tpeCase.attributes) { att =>
              forApproach.dispatch(SendRequest(
                onRequest.attributes(att),
                math.J3.getModel.baseDataType,
                onRequest.request
              ))
            }
          } yield atts

          val realResult = onRequest.request.op match {
            case math.M0.Eval =>
              onRequest.tpeCase match {
                case math.J3.Divd => {
                  for {
                    atts <- attsGen
                    res <- div(atts: _*)
                  } yield res
                }
                case math.J3.Neg =>
                  for {
                    atts <- attsGen
                    minusOne <- reify(TypeRep.Double, -1)
                    res <- mult(minusOne, atts.head)
                  } yield res
                case _ => ???
              }

            case math.J3.PrettyP =>
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
                case math.J1.Sub => {
                  for {
                    atts <- attsGen
                    res <- makeString(atts, "(", "-", ")")
                  } yield res
                }
                case math.J2.Mult => {
                  for {
                    atts <- attsGen
                    res <- makeString(atts, "(", "*", ")")
                  } yield res
                }
                case math.J3.Divd => {
                  for {
                    atts <- attsGen
                    res <- makeString(atts, "(", "/", ")")
                  } yield res
                }
                case math.J3.Neg =>
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
        if (onRequest.request.op != math.J2.isOp(onRequest.tpeCase) && onRequest.request.op.tags.contains(math.J2.IsOp)) {
          import ffiBoolean.booleanCapabilities._
          for {
            booleanFalse <- falseExp
          } yield Some(booleanFalse)
        } else if (onRequest.request.op == math.J2.isOp(onRequest.tpeCase)) {
          genericLogic(forApproach)(onRequest)  // same isOpTypeCase applied to TypeCase can pass in
        } else {
          // if opname is a "isSub" or "isAdd" for older typecase, but we are in newest one? Still send to generic logic
          val pastOps = math.J2.isOps(model.flatten.typeCases)
          if (pastOps.contains(onRequest.request.op)) {
            genericLogic(forApproach)(onRequest)
          } else if (onRequest.request.op == math.J1.MultBy) {
            /* Handle MultBy with these data types. */
            onRequest.tpeCase match {
              case divd@math.J3.Divd =>
                for {
                 // this.left.multby(other)
                  numerator <- forApproach.dispatch(
                    SendRequest(
                      onRequest.attributes.toSeq.head._2,
                      onRequest.onType,
                      onRequest.request,    // multby
                    )
                  )

                  res <- forApproach.instantiate(math.M0.getModel.baseDataType, math.J3.Divd, numerator, onRequest.attributes(Attribute.right(model)))
                } yield Some(res)

              case math.J3.Neg => genericLogic(forApproach)(onRequest)
              case _ => ???
            }
          } else if (onRequest.request.op == math.J2.Eql) {
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
