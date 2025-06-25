package org.combinators.ep.domain.shape.eips      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Booleans, RealArithmetic, Strings}
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
import org.combinators.ep.domain.{GenericModel, shape}

/**
 * Truly language independent abstractions.  Since we are in the mathematical domain, the
 * ffi package contains mathematical concerns, which are then completely abstract from the underlying
 * language in which the code generator works.
 *
 * Introducing Lit/Add  and Eval.
 *
 * FFI allows this object to specify
 *
 * Doing this here using FFIs allows us to avoid having to create/duplicate nearly identical specifications
 * just to work with different languages, i.e., Java/e0 and then GJ/e0 and CPP/e0
 *
 * There should be many similarities between Java-generator and CPP-generator. I used a "poor man's version"
 * of modeling for C++ code. Move that content into paradigm
 */
object S0 {
  def apply[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
  (paradigm: P)
  (ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double],
   ffiBooleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
   ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  ):
  EvolutionImplementationProvider[AIP[paradigm.type]] =
    new EvolutionImplementationProvider[AIP[paradigm.type]] {
      val model:GenericModel = shape.S0.getModel

      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiArithmetic.enable()
          _ <- ffiBooleans.enable()
          _ <- ffiRealArithmetic.enable()
          _ <- ffiStrings.enable()
        } yield ()
      }

      override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
        if ((potentialRequest.op == shape.S0.ContainsPt) &&
          Set(shape.S0.Circle, shape.S0.Square, shape.S0.Translate).contains(potentialRequest.tpeCase)) {
          Some(Set.empty)
        } else {
          None
        }
      }

      override def logic
      (forApproach: AIP[paradigm.type])
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ffiArithmetic.arithmeticCapabilities._
        import ffiBooleans.booleanCapabilities._
        import ffiRealArithmetic.realArithmeticCapabilities._

        assert(dependencies(PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)).nonEmpty)

        onRequest.request.op match {
          case op if op == shape.S0.ContainsPt =>
            {
              val result = onRequest.tpeCase match {

                /**
                 *
                 */
                case squareC@shape.S0.Square =>
                  for {
                    // return (Math.abs(point.x) <= side / 2 && Math.abs(point.y) <= side / 2);
                    sqPtX <- mult(onRequest.request.arguments(shape.S0.pointx), onRequest.request.arguments(shape.S0.pointx))
                    sqPtY <- mult(onRequest.request.arguments(shape.S0.pointy), onRequest.request.arguments(shape.S0.pointy))
                    four <- forApproach.reify(InstanceRep(TypeRep.Double)(4.0))
                    sideSq <- mult(onRequest.attributes(shape.S0.side), onRequest.attributes(shape.S0.side))
                    sideDiv4 <- div(sideSq,four)
                    compX <- le(sqPtX, sideDiv4)
                    compY <- le(sqPtY, sideDiv4)
                    res <- and(Seq(compX, compY))
                  } yield res

                case circleC@shape.S0.Circle =>
                  for {
                    //  return Math.sqrt(point.x * point.x + point.y * point.y) <= radius;
                    sqPtX <- mult(onRequest.request.arguments(shape.S0.pointx), onRequest.request.arguments(shape.S0.pointx))
                    sqPtY <- mult(onRequest.request.arguments(shape.S0.pointy), onRequest.request.arguments(shape.S0.pointy))
                    total <- add(sqPtX, sqPtY)
                    root <- sqrt(total)
                    res <- le(root, onRequest.attributes(shape.S0.radius))
                  } yield res

                /** Need to dispatch 'containsPt' to the inner after normalizing (x,y) accordingly. */
                case translateC@shape.S0.Translate =>
                  for {
                    normx <- sub(onRequest.request.arguments(shape.S0.pointx), onRequest.attributes(shape.S0.transx))
                    normy <- sub(onRequest.request.arguments(shape.S0.pointy), onRequest.attributes(shape.S0.transy))

                    inner <- forApproach.dispatch(SendRequest(
                      onRequest.attributes(shape.S0.shape),  // instead use look-up addC.attributes.find(att => att.name)
                      shape.S0.getModel.baseDataType,
                      Request(shape.S0.ContainsPt, Map(shape.S0.pointx -> normx, shape.S0.pointy -> normy))         // must be changed in response to translate
                      ))

                  } yield inner

                // Scala response to ultimately cause runtime exception
                case _ => ???
              }
              result.map(Some(_))
            }
        }


      }
    }
}