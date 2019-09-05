import org.combinators.ep.domain.Model
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, EvolutionImplementationProvider, NameProvider, communication}
import Command.{skip, _}
import cats.syntax._
import cats.implicits._
import org.combinators.ep.domain.abstractions.{Attribute, DataType, DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.{AddImport, Apply, FindMethod, Functional, ResolveImport}

trait Traditional extends ApproachImplementationProvider {
  val names: NameProvider
  val functional: Functional.WithBase[paradigm.type]

  import paradigm._
  import functional._
  import syntax._

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import functional.methodBodyCapabilities._

    for {
      method <- FindMethod[Expression](names.instanceNameOf(message.request.op)).interpret
      mi <- ResolveImport[Import, Expression](method).interpret
      _ <- mi.map(AddImport(_).interpret).getOrElse(skip)
      res <- Apply(method, message.to +: message.request.op.parameters.map(message.request.arguments)).interpret
    } yield res
  }

  /** Produces all compilation units necessary to implement the given model.
    * Fills in domain specific code with the given approach independent [[EvolutionImplementationProvider]].
    */
  override def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[Traditional.this.type]): Seq[paradigm.syntax.CompilationUnit] = ???
}
