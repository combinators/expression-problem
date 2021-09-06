package org.combinators.ep.generator    /*DI:LI:AI*/

import org.combinators.ep.domain._
import abstractions._
import communication._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}

import org.combinators.ep.generator.Command._
import org.combinators.ep.generator.paradigm.{AddImport, AnyParadigm, ResolveImport}
import AnyParadigm.syntax._

/** Provides implementations for language and approach specific code generation tasks which do not depend on a specific
  * EP domain. */
trait ApproachImplementationProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
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
    *   CodeBlockWithResultingExpressions()(
    *     Java(s"${received.selfReference}.${Attribute.left.name}.eval()").expression())
    *   )
    * }}}
   *
    */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression]

  /** Returns code to instantiate the given data type case, filling in `args` for its parameters. */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression]

  /** Returns code to instantiate the given Scala model of a domain specific type. */
  def instantiate(baseType: DataType, inst: DataTypeInstance): Generator[MethodBodyContext, Expression] = {
    for {
      attributeInstances <- forEach (inst.attributeInstances) { ati => reify(ati) }
      result <- instantiate(baseType, inst.tpeCase, attributeInstances: _*)
    } yield result
  }

  /** Available in any Context that can ResolveImport and AddImport. */
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
          resTy <- toTargetLanguageType(tpe)
          _ <- resolveAndAddImport(resTy)
          res <- methodBodyCapabilities.reify[tpe.HostType](tpe, inst.asInstanceOf[tpe.HostType])
        } yield res
      case _ => throw new scala.NotImplementedError(s"No rule to compile instantiations of ${inst.tpe}.")
    }
  }

  /** Adds everything necessary to implement the given model to the project context.
    * Fills in domain specific code with the given approach independent [[EvolutionImplementationProvider]].
    */
  def implement(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit]

  /** Define standard test name. */
  def testName:Name = {
    names.mangle("Test")
  }

  /** Define name for test case, given model. */
  def testCaseName(model:GenericModel):Name = {
    names.addSuffix(names.mangle(names.conceptNameOf(model)), "Test")
  }

  /** When test case class needs to extend another class, override this. */
  def testCaseParent(model:GenericModel):Seq[Type] = Seq.empty

  /** When test case class needs to implementsome interfaces, override this. */
  def testCaseImplements(model:GenericModel):Seq[Type] = Seq.empty

  /** Adds tests to the project context */
  def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import projectContextCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.testCapabilities._
    for {
      _ <- testImplementationProvider.initialize(this)
      _ <-
        forEach(tests.toList) { case (model, tests) =>
          val testCode: Generator[MethodBodyContext, Seq[Expression]] =
            for {
              code <- forEach(tests) { test => testImplementationProvider.test(this)(test) }
            } yield code.flatten

          val tyz:Generator[CompilationUnitContext, Unit]  = addTestSuite(testCaseName(model), addTestCase(testCode, testName))

          // TODO: How to take advantage of new capability AddImplementedInTests???

          addCompilationUnit(
            addTestSuite(
              testCaseName(model),
              addTestCase(testCode, testName)
            ),
            testCaseName(model))
        }
    } yield ()
  }
}

object ApproachImplementationProvider {
  type WithParadigm[P <: AnyParadigm] = ApproachImplementationProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]
}