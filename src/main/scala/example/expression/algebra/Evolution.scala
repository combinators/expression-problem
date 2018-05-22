package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import example.expression.{ExpressionDomain, _}
import expression.history.History
import expression.tests.AllTests
import javax.inject.Inject
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends InhabitationController(web, app) with RoutingEntries {

  lazy val controllerAddress = "e0"

  // all tests are derived from the model.
  lazy val tests_e0 = tests.e0.TestCases.add(new AllTests())
  lazy val history_e0 = evolution.E0.extend(new History)

  lazy val rep_e0 = new ExpressionDomain(history_e0, tests_e0) with ExpressionSynthesis with Structure with Registry with e0.Model
  lazy val Gamma_e0 = rep_e0.init(ReflectedRepository(rep_e0, classLoader = this.getClass.getClassLoader), history_e0)
  import rep_e0._

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma_e0.combinatorComponents

  /** Has to be lazy so subclasses can compute model. */
  lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e0).addJob[CompilationUnit](Constructor("HACK")).compute()

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("algebra")
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E0_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e1"
  // all tests are derived from the model.
  lazy val tests_e1 = tests.e1.TestCases.add(tests_e0)
  lazy val history_e1 = evolution.E1.extend(history_e0)

  //override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e1, controllerAddress)
  lazy val rep_e1 = new ExpressionDomain(history_e1, tests_e1) with ExpressionSynthesis with Structure with Registry with e0.Model with e1.Model
  lazy val Gamma_e1 = rep_e1.init(ReflectedRepository(rep_e1, classLoader = this.getClass.getClassLoader), history_e1)

  /** This needs to be defined, and it is set from Gamma. */
  override lazy val combinatorComponents = Gamma_e1.combinatorComponents
  override lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e1).addJob[CompilationUnit](Constructor("HACK")).compute()
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E1_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e2"
  // all tests are derived from the model.
  lazy val tests_e2 = tests.e2.TestCases.add(tests_e1)
  lazy val history_e2 = evolution.E2.extend(history_e1)

  lazy val rep_e2 = new ExpressionDomain(history_e2, tests_e2) with ExpressionSynthesis with Structure with Registry with e0.Model with e1.Model with e2.Model
  lazy val Gamma_e2 = rep_e2.init(ReflectedRepository(rep_e2, classLoader = this.getClass.getClassLoader), history_e2)
  override lazy val combinatorComponents = Gamma_e2.combinatorComponents
  override lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e2).addJob[CompilationUnit](Constructor("HACK")).compute()
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E2_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e3"
  // all tests are derived from the model.
  lazy val tests_e3 = tests.e3.TestCases.add(tests_e2)
  lazy val history_e3 = evolution.E3.extend(history_e2)

  lazy val rep_e3 = new ExpressionDomain(history_e3, tests_e3) with ExpressionSynthesis with Structure with Registry with e0.Model with e1.Model with e2.Model with e3.Model
  lazy val Gamma_e3 = rep_e3.init(ReflectedRepository(rep_e3, classLoader = this.getClass.getClassLoader), history_e3)
  override lazy val combinatorComponents = Gamma_e3.combinatorComponents
  override lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e3).addJob[CompilationUnit](Constructor("HACK")).compute()
}

