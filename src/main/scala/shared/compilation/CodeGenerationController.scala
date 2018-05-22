package shared.compilation

import com.github.javaparser.ast.CompilationUnit
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.persistable.Persistable
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import play.api.mvc.{Action, AnyContent}

abstract class CodeGenerationController[ResultType] (web: WebJarsUtil, app: ApplicationLifecycle)
  (implicit resultPersistence: Persistable.Aux[ResultType])
  extends InhabitationController(web, app) with RoutingEntries {

  val generatedCode: Seq[ResultType]

  /** Uses an empty repository dummy */
  lazy val Gamma = ReflectedRepository((), classLoader = this.getClass.getClassLoader)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  /** Has to be lazy so subclasses can compute model. */
  lazy val results:Results = generatedCode.foldLeft(EmptyInhabitationBatchJobResults(Gamma).compute()){
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

