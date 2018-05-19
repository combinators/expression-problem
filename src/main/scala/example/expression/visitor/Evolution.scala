package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import example.expression.ExpressionDomain
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.{DomainModel, Exp, Operation}
import expression.instances.UnitSuite
import expression.operations.SimplifyExpr
import expression.tests.AllTests
import javax.inject.Inject
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.templating.persistable.JavaPersistable._

import scala.collection.JavaConverters._

class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends InhabitationController(web, app) with RoutingEntries {

  lazy val controllerAddress = "e0"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  def model():DomainModel = new DomainModel(
    List[Exp](new Lit, new Add).asJava,
    List[Operation](new Eval).asJava
  )

  // all tests are derived from the model.
  lazy val tests_e0 = e0.TestCases.add(new AllTests())

  lazy val rep = new ExpressionDomain(model(), tests_e0) with ExpressionSynthesis with e0.Model with InitializeRepository {}

  lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), model())

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  /** Has to be lazy so subclasses can compute model. */
  lazy val targets:Seq[Constructor] = Synthesizer.visitorTargets(model())
  lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma).addJobs[CompilationUnit](targets).compute()

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("visitor")
}



class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E0_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e1"

  lazy val tests_e1:UnitSuite = e1.TestCases.add(tests_e0)

  override def model():DomainModel = super.model().merge(new DomainModel(
    List[Exp](new Sub).asJava,
    List.empty.asJava
  ))

  override lazy val rep = new ExpressionDomain(model(), tests_e1) with ExpressionSynthesis with e1.Model with e0.Model with InitializeRepository {}
}



class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E1_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e2"

  lazy val tests_e2:UnitSuite = e2.TestCases.add(tests_e1)

  override def model():DomainModel = super.model().merge(new DomainModel(
    List.empty.asJava,
    List[Operation](new PrettyP).asJava,
  ))

  override lazy val rep = new ExpressionDomain(model(), tests_e2) with ExpressionSynthesis with e2.Model with e1.Model with e0.Model with InitializeRepository {}
}



class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E2_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e3"

  val tests_e3:UnitSuite = e3.TestCases.add(tests_e2)

  override def model():DomainModel = super.model().merge(new DomainModel(
    List[Exp](new Neg, new Mult, new Divd).asJava,
    List.empty.asJava
  ))

  override lazy val rep = new ExpressionDomain(model(), tests_e3) with ExpressionSynthesis with e3.Model with e2.Model with e1.Model with e0.Model with InitializeRepository {}
}




class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E3_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e4"

  val tests_e4:UnitSuite = e4.TestCases.add(tests_e3)

  override def model():DomainModel = super.model().merge(new DomainModel(
    List.empty.asJava,
    List[Operation](new Collect, new SimplifyExpr).asJava
  ))

  override lazy val rep = new ExpressionDomain(model(), tests_e4) with ExpressionSynthesis with e4.Model with e3.Model with e2.Model with e1.Model with e0.Model with InitializeRepository {}
}