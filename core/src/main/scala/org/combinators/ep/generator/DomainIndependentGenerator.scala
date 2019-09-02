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
import org.combinators.ep.generator.paradigm.AnyParadigm



/** Provides implementations for language and approach specific code generation tasks which do not depend on a specific
  * EP domain. */
abstract class DomainIndependentGenerator[S <: AbstractSyntax](val syntax: AbstractSyntax, val paradigm: AnyParadigm[S])  {
  import syntax._
  import paradigm._





  // was toTargetLanguage --> now instantiate


  /** Produces the sequence of statements required to return `expr` as the result of some operation.
    *
    * In some languages, this result needs to be returned (i.e., Java or C++); in
    * some EP approaches, this result is simply stored (i.e., see C++ implementation of visitorTable).
    * In other languages (i.e., Scala or Haskell) there is no specific return statement, effectively turning this
    * into an identity function.
    */
  def toOperationResult(expr: Expression): Seq[Statement]

  /** Converts a code block to a sequence of statements representing the body of a method returning its
    * resulting expression.
    *
    * @see [[toOperationResult()]] for the generation of code returning the resulting expression.
    */
  def toMethodBody(block: Command.Generator[MethodBodyContext, Expression]): Seq[Statement] = {
    val compiler = new (Command.Generator[MethodBodyContext, *] ~> State[Seq[Statement], *]) {
      def apply[A](fa: Command[MethodBodyContext, A]): State[Seq[Statement], A] = {
        fa match {
          case AddContext(stmts: Seq[Statement]) => State.modify[Seq[Statement]](_ ++ stmts).map(_.asInstanceOf[A])
          case AddImport(_) => State.pure(().asInstanceOf[A])
        }
      }
    }
    val (context, resultExp) = block.foldMap(compiler).run(Seq.empty).value
    context ++ toOperationResult(resultExp)
  }

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
  def dispatch(message: SendRequest[Expression]): CodeBlockWithResultingExpressions[Statement, Expression]

  /** Produces all compilation units necessary to implement the given model.
    * Fills in domain specific code with the given approach independent [[EvolutionImplementationProvider]].
    */
  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[S]): Seq[CompilationUnit]
}