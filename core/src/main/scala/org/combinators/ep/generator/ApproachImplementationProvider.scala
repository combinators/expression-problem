package org.combinators.ep.generator    /*DI:LI:AI*/

import org.combinators.ep.domain._
import abstractions._
import communication._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import cats._
import cats.data._
import cats.syntax._
import cats.implicits._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator.paradigm.{AddImport, AnyParadigm, Reify, ResolveImport, ToTargetLanguageType}

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

  /** Returns code to instantiate the given data type case, filling in `args` for its parameters. */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression]

  /** Returns code to instantiate the given Scala model of a domain specific type. */
  def instantiate(baseType: DataType, inst: DataTypeInstance): Generator[MethodBodyContext, Expression] = {
    for {
      attributeInstances <- inst.attributeInstances.toList.map(reify).sequence[Generator[MethodBodyContext, *], Expression]
      result <- instantiate(baseType, inst.tpeCase, attributeInstances: _*)
    } yield result
  }

  def resolveAndAddImport[Context, Elem](elem: Elem)
    (implicit
      canResolveImport: Understands[Context, ResolveImport[Import, Elem]],
      canAddImport: Understands[Context, AddImport[Import]]
    ) : Generator[Context, Unit] = {
    ResolveImport[Import, Elem](elem).interpret.flatMap(imp => imp.map(AddImport(_).interpret).getOrElse(skip))
  }

  /** Converts a Scala model of an instance of any representable type into code. */
  def reify(inst: InstanceRep): Generator[MethodBodyContext, Expression] = {
    (inst.tpe, inst.inst) match {
      case (TypeRep.DataType(baseTpe), domInst: DataTypeInstance) => instantiate(baseTpe, domInst)
      case (tpe, inst) =>
        import paradigm.methodBodyCapabilities._
        for {
          resTy <- ToTargetLanguageType[Type](tpe).interpret
          _ <- resolveAndAddImport(resTy)
          res <- Reify[tpe.HostType, Expression](tpe, inst.asInstanceOf[tpe.HostType]).interpret
        } yield res
      case _ => throw new scala.NotImplementedError(s"No rule to compile instantiations of ${inst.tpe}.")
    }
  }

  /** Produces all compilation units necessary to implement the given model.
    * Fills in domain specific code with the given approach independent [[EvolutionImplementationProvider]].
    */
  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Seq[CompilationUnit]

}

object ApproachImplementationProvider {
  type WithParadigm[P <: AnyParadigm] = ApproachImplementationProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]
}