package org.combinators.ep.generator    /*DI:LI:AI*/

import org.combinators.ep.domain._
import abstractions._
import communication._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import cats._
import cats.data._
import cats.implicits._
import cats.instances._
import cats.free.FreeApplicative._
import cats.free.Free.catsFreeMonadForId
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm


/** Provides implementations for language and approach specific code generation tasks which do not depend on a specific
  * EP domain. */
trait ApproachImplementationProvider {
  val paradigm: AnyParadigm
  import paradigm._
  import syntax._

  /** Produces the code to dispatch a request using the target language and approach specific message delivery mechanism.
    *
    * For example sending "eval" to the left attribute in Java and the OO approach:
    * {{{
    *   implicit val domain = ???
    *   val received: ReceivedRequest[Expression] = ???
    *   dispatch(
    *     SendRequest(
    *       to = received.attributes(Attribute.left),
    *       receiverTpe = Attribute.left.tpe
    *       request = Request(Operation("eval"), Map.empty)
    *       inReplyTo = Some(received)
    *   )) // Results in:
    *   CodeBlockWithResultingExpresions()(
    *     Java(s"${received.selfReference}.${Attribute.left.name}.eval()").expression())
    *   )
    * }}}
    */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression]

  /** Produces all compilation units necessary to implement the given model.
    * Fills in domain specific code with the given approach independent [[EvolutionImplementationProvider]].
    */
  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider.WithSyntax[syntax.type]): Seq[CompilationUnit]

}

object ApproachImplementationProvider {
  type WithParadigm[P <: AnyParadigm] = ApproachImplementationProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = ApproachImplementationProvider { val paradigm: AnyParadigm.WithSyntax[S] }
}