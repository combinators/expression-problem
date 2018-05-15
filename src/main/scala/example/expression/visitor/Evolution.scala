package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import expression.DomainModel
import expression.evolution.Development
import expression.history.History
import expression.instances.UnitSuite
import expression.tests.AllTests
import javax.inject.Inject
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

import scala.collection.JavaConverters._
import org.combinators.templating.persistable.JavaPersistable._

abstract class Evolution(web: WebJarsUtil, app: ApplicationLifecycle)
  extends InhabitationController(web, app) with RoutingEntries {

  // identify the desired evolution (e0, e1, e2, ...)
  val evolution: String
  val controllerAddress: String
  val model: DomainModel

  // FreeCellDomain is base class for the solitaire variation. Note that this
  // class is used (essentially) as a placeholder for the solitaire val,
  // which can then be referred to anywhere as needed.
  lazy val allTests : UnitSuite = new AllTests(model)

  lazy val repository = new ExpressionSynthesis(model, allTests) with InitializeRepository {}
  import repository._

  lazy val Gamma = repository.init(ReflectedRepository(repository, classLoader = this.getClass.getClassLoader), model)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  val targets:Seq[Constructor] = Synthesizer.visitorTargets(domain)
  lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma).addJobs[CompilationUnit](targets).compute()

  override val routingPrefix: Option[String] = Some("visitor")
}

class E4_EvolutionController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle)
  extends Evolution(webJars, applicationLifecycle) {

  lazy val evolution = "e4"
  lazy val controllerAddress = "e4"

  lazy val model:DomainModel = Development.base.iterator(evolution).asScala.foldLeft(new DomainModel()) {
    case (m, sub) => m.merge(sub)
  }
}

class E3_EvolutionController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle)
  extends Evolution(webJars, applicationLifecycle) {

  lazy val evolution = "e3"
  lazy val controllerAddress = "e3"

  lazy val model:DomainModel = Development.base.iterator(evolution).asScala.foldLeft(new DomainModel()) {
    case (m, sub) => m.merge(sub)
  }
}

class E2_EvolutionController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle)
  extends Evolution(webJars, applicationLifecycle) {

  lazy val evolution = "e2"
  lazy val controllerAddress = "e2"

  lazy val model:DomainModel = Development.base.iterator(evolution).asScala.foldLeft(new DomainModel()) {
    case (m, sub) => m.merge(sub)
  }
}

class E1_EvolutionController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle)
  extends Evolution(webJars, applicationLifecycle) {

  lazy val evolution = "e1"
  lazy val controllerAddress = "e1"

  lazy val model:DomainModel = Development.base.iterator(evolution).asScala.foldLeft(new DomainModel()) {
    case (m, sub) => m.merge(sub)
  }
}

class E0_EvolutionController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle)
  extends Evolution(webJars, applicationLifecycle) {

  lazy val evolution = "e0"
  lazy val controllerAddress = "e0"

  lazy val model:DomainModel = Development.base.iterator(evolution).asScala.foldLeft(new DomainModel()) {
    case (m, sub) => m.merge(sub)
  }
}