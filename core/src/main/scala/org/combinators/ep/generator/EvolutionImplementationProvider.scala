package org.combinators.ep.generator   /*DI:LI:AI*/

import cats.kernel.Monoid
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm

/** Instances of this class provide the domain dependent implementation of an evolution. */
trait EvolutionImplementationProvider[-AIP <: ApproachImplementationProvider] {

  val model: GenericModel

  /** Initializes the project context to support this evolution, e.g. by calling
    * [[org.combinators.ep.generator.paradigm.ffi.FFI.enable()]] for all the required FFIs.
    */
  def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit]

//  /** Accesses API with PotentialRequest derived from onRequest. */
//  def applicableIn(forApproach: AIP)(onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression], currentModel:GenericModel): Option[GenericModel] =
//    applicableIn(forApproach, PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op), currentModel)
//
//  /** For more complicated ExtensionGraphs, this returns most appropriate evolution for which an EIP is available.
//   * In linear histories, this is the most recent. For histories involving merging, the EIP is responsible
//   * for choosing which branch(es) to forward request to. Takes care to check against "current model" to
//   * avoid returning an implementation for the future. */
//  def applicableIn(forApproach: AIP, potentialRequest: PotentialRequest, currentModel:GenericModel): Option[GenericModel] =
//    if ((model == currentModel || model.before(currentModel)) && applicable(forApproach, potentialRequest)) {
//      Some(model)
//    } else {
//      None
//    }
//
//  /** Tests if this evolution implementation provider is applicable for the given request */
//  def applicable(forApproach: AIP)(onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean =
//    applicable(forApproach, PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op))
//
//  /** Tests if this evolution implementation provider is applicable for the given request */
//  def applicable(forApproach: AIP, onRequest: PotentialRequest): Boolean
//
//  /** Can vary by operation and data type. */
//  @Deprecated def dependencies(op:Operation, dt:DataTypeCase) : Option[Set[Operation]] = None

  def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = None

//  @Deprecated def evolutionSpecificDependencies(op: Operation, dt: DataTypeCase): Map[GenericModel, Set[Operation]] =
//    dependencies(op, dt).map(deps => Map(model -> deps)).getOrElse(Map.empty)

  def evolutionSpecificDependencies(potentialRequest: PotentialRequest): Map[GenericModel, Set[Operation]] =
    dependencies(potentialRequest).map(deps => Map(model -> deps)).getOrElse(Map.empty)

  /** Default logic can be defined for any operation that suggests the potential for Write Once Use Anywhere.
   * If we get here, no generic logic was defined. */
  def genericLogic
    (forApproach: AIP)
    (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
    Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = ???

  /** Default logic can be defined for any operation that suggests the potential for Write Once Use Anywhere.
   * Use this default Generic Logic when you simply want to dispatch the operation for onRequest to each of
   *  its children attributes. */
  def defaultGenericLogic
    (forApproach: AIP)
    (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
  Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
    import AnyParadigm.syntax._

    onRequest.tpeCase match {

      // handles every DT with an arbitrary number of attributes. This works because we have the model, and
      // the dispatch already abstracts over all requests.
      case dt =>
        for {
          processedAtts <- forEach (dt.attributes) {
            att => forApproach.dispatch(SendRequest(
              onRequest.attributes(att),
              model.baseDataType,
              onRequest.request
              ))
          }

          res <- forApproach.instantiate(model.baseDataType, dt, processedAtts : _ *)
        } yield Some(res)
    }
  }

  /** Generates the code of request handlers relative to the target language and approach specific code generation
    * logic provided by the given `codeGenerator`. */
  def logic
      (forApproach: AIP)
      (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
    Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]]
}

object EvolutionImplementationProvider {

  /** Allows to combine multiple [[EvolutionImplementationProvider]] objects into one. */
  implicit def monoidInstance[AIP <: ApproachImplementationProvider]: Monoid[EvolutionImplementationProvider[AIP]] =
    new Monoid[EvolutionImplementationProvider[AIP]] {
      /** Returns an [[EvolutionImplementationProvider]] which does not provide any implementation, and instead fails
        * with a runtime exception */
      def empty: EvolutionImplementationProvider[AIP] = new EvolutionImplementationProvider[AIP] {
        override val model:GenericModel = ???  // won't ever access
        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = Command.skip
//        override def applicableIn
//           (forApproach: AIP)
//           (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression], current:GenericModel): Option[GenericModel] = None
//        override def applicableIn(forApproach: AIP, onRequest: PotentialRequest, current:GenericModel): Option[GenericModel] = None
//
//        override def applicable
//           (forApproach: AIP)
//           (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]): Boolean = false
//        override def applicable
//        (forApproach: AIP, onRequest: PotentialRequest): Boolean = false
        override def genericLogic
           (forApproach: AIP)
           (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]) =
          throw new RuntimeException(s"No generic logic to handle request ${onRequest}")
        def logic
           (forApproach: AIP)
           (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]) =
          throw new RuntimeException(s"No logic to handle request ${onRequest}")
      }

      /** Combines two [[EvolutionImplementationProvider]] objects by trying to resolve requests with the first provided
        * logic, resorting to the second logic on failure. */
      def combine(
          first: EvolutionImplementationProvider[AIP],
          second: EvolutionImplementationProvider[AIP]
        ): EvolutionImplementationProvider[AIP] = new EvolutionImplementationProvider[AIP] {

        // bias is to use the first
        override val model = first.model

//        @Deprecated override def dependencies(op: Operation, dt: DataTypeCase): Option[Set[Operation]] = {
//          (first.dependencies(op, dt), second.dependencies(op, dt)) match {
//            case (None, None) => None
//            case (Some(deps1), Some(deps2)) => Some(deps1 ++ deps2)
//            case (Some(deps1), _) => Some(deps1)
//            case (_, deps) => deps
//          }
//        }
//
//        /** Ensure dependencies are union'd through composition. */
//        @Deprecated override def evolutionSpecificDependencies(op:Operation, dt:DataTypeCase) : Map[GenericModel, Set[Operation]] =
//          second.evolutionSpecificDependencies(op, dt) ++ first.evolutionSpecificDependencies(op, dt)

        override def dependencies(potentialRequest: PotentialRequest): Option[Set[Operation]] = {
          (first.dependencies(potentialRequest), second.dependencies(potentialRequest)) match {
            case (None, None) => None
            case (Some(deps1), Some(deps2)) => Some(deps1 ++ deps2)
            case (Some(deps1), _) => Some(deps1)
            case (_, deps) => deps
          }
        }

        /** Ensure dependencies are union'd through composition. */
        override def evolutionSpecificDependencies(potentialRequest: PotentialRequest): Map[GenericModel, Set[Operation]] = {
          val result = second.evolutionSpecificDependencies(potentialRequest) ++ first.evolutionSpecificDependencies(potentialRequest)
          if (model.name == "o1oa" && potentialRequest.op.name == "eval") {
            println(potentialRequest)
            println(result)
          }
          // make sure that all models that are implemented appear in the result
          assert(model.haveImplementation(potentialRequest).forall(m => result.contains(m)))

          //assert(result.keys == model.haveImplementation(potentialRequest))

          result
        }

        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = {
          for {
            _ <- first.initialize(forApproach)
            _ <- second.initialize(forApproach)
          } yield ()
        }

        /** Do not call applicable, but rather check to see if genericLogic is in play. */
        override def genericLogic
            (forApproach: AIP)
            (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
          try {
            first.genericLogic(forApproach)(onRequest)
          } catch {
            case _:RuntimeException | _:NotImplementedError => second.genericLogic(forApproach)(onRequest)
          }
        }

        /** Logic is attempted on 'first' (if applicable), otherwise sent to second. */
        def logic
            (forApproach: AIP)
            (onRequest: ReceivedRequest[forApproach.paradigm.syntax.Expression]):
        Generator[forApproach.paradigm.MethodBodyContext, Option[forApproach.paradigm.syntax.Expression]] = {
          val potentialRequest = PotentialRequest(onRequest.onType, onRequest.tpeCase, onRequest.request.op)
          val dependenciesInFirst = first.evolutionSpecificDependencies(potentialRequest)
          val firstKeys = dependenciesInFirst.keySet
          def check(model: GenericModel): GenericModel => Boolean = m => {
           //m.beforeOrEqual(model)
            m.equals(model)   // FIND IT explicitly. Needed to handle O1OA for multiple approaches.
          }
          onRequest.model match {
            case None if dependenciesInFirst.nonEmpty => first.logic(forApproach)(onRequest)

            // if we HAVE an onrequest.model, just find the corresponding EIP

            case Some(model) if firstKeys.exists(check(model)) => {
              first.logic(forApproach)(onRequest)
            }
            case _ => second.logic(forApproach)(onRequest)
          }

//          if (first.applicable(forApproach)(onRequest)) {
//            first.logic(forApproach)(onRequest)
//          } else {
//            second.logic(forApproach)(onRequest)
//          }
        }
      }
    }
}
