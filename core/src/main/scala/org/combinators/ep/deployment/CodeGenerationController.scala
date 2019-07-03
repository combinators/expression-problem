package org.combinators.ep.deployment   /*DI:LI:AI*/

import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.persistable.Persistable
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import play.api.mvc.{Action, AnyContent}

/**
  * This class is the foundation of any EP code generation strategy.
  *
  * For each EP strategy, develop a subclass, for example, one possibility is shown below:
  *
  * {{{
  *   abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  *       extends CodeGenerationController[CompilationUnit](web, app) {
  *
  *     val gen:WithDomain[MathDomain] with OOGenerator with JUnitTestGenerator
  *
  *     override lazy val generatedCode:Seq[CompilationUnit] =
  *            gen.generatedCode() ++
  *            gen.generateSuite(routingPrefix)
  *
  *     override val routingPrefix: Option[String] = Some("oo")
  *     override lazy val controllerAddress:String = gen.getModel.name
  *   }
  * }}}
  *
  * The routingPrefix is used to uniquely determine this EP approach, and this can also be used
  * within generateSuite as the top-level Java package.
  *
  * The controllerAddress is the evolution name, such as "m2" or "m4", based upon the desired
  * evolution.
  *
  * @tparam ResultType         Persistable unit containing code fragments in target programming language
  */
abstract class CodeGenerationController[ResultType] (web: WebJarsUtil, app: ApplicationLifecycle)
  (implicit resultPersistence: Persistable.Aux[ResultType])
  extends InhabitationController(web, app) with RoutingEntries {

  /** Uses an empty repository dummy */
  lazy val Gamma = ReflectedRepository((), classLoader = this.getClass.getClassLoader)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  val generatedCode:Seq[ResultType]
  lazy val results:Results = defaultResults(generatedCode)

  /** Has to be lazy so subclasses can compute model. */
  def defaultResults(genCode:Seq[ResultType]) : Results =  genCode.foldLeft(EmptyInhabitationBatchJobResults(Gamma).compute()) {
    (result, code) => result.addExternalArtifact[ResultType](code)
  }

  /** Always prepares result 0 before checking out */
  override def serveFile(name: String): Action[AnyContent] = {
    implicit val ex = defaultExecutionContext
    Action.async(request =>
      super.prepare(0)(request).flatMap(_ => super.serveFile(name)(request))
    )
  }
}
