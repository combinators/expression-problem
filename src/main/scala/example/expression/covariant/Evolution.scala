package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import example.expression.ExpressionDomain
import example.expression._
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.history.History
import expression.operations.SimplifyExpr
import expression.tests.AllTests
import expression.{DomainModel, Exp, Operation}
import javax.inject.Inject
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import example.expression.tests._


class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends InhabitationController(web, app) with RoutingEntries {

  lazy val controllerAddress = "e0"

//  val history:History = new History
//  history.extend("e0", new DomainModel(
//    List[Exp](new Lit, new Add).asJava,
//    List[Operation](new Eval).asJava
//  ))
//
//  // evolution 1 (from Extensibility for the Masses example)
//  history.extend("e1",  new DomainModel(
//    List[Exp](new Sub).asJava,
//    List.empty.asJava
//  ))
//
//  // evolution 2 (from Extensibility for the Masses example)
//  history.extend("e2",  new DomainModel(
//    List.empty.asJava,
//    List[Operation](new PrettyP).asJava
//  ))
//
//  // Evolution 1: Extension to domain model has new data variants and operations
//  history.extend("e3",  new DomainModel(
//    List[Exp](new Neg, new Mult, new Divd).asJava,
//    List.empty.asJava
//  ))
//
//  history.extend("e4",  new DomainModel(
//    List.empty.asJava,
//    List[Operation](new Collect, new SimplifyExpr).asJava
//  ))

  // all tests are derived from the model.
  lazy val tests_e0 = e0.TestCases.add(new AllTests())
  lazy val history_e0 = evolution.E0.extend(new History)

  lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e0, controllerAddress)
  lazy val rep = new ExpressionDomain(history_e0, tests_e0) with ExpressionSynthesis with Structure
  lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e0)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  /** Has to be lazy so subclasses can compute model. */
  lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma).addJobs[CompilationUnit](targets).compute()

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("covariant")
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E0_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e1"
  // all tests are derived from the model.
  lazy val tests_e1 = e1.TestCases.add(tests_e0)
  lazy val history_e1 = evolution.E1.extend(history_e0)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e1, controllerAddress)
  override lazy val rep = new ExpressionDomain(history_e1, tests_e1) with ExpressionSynthesis with Structure
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e1)
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E1_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e2"
  // all tests are derived from the model.
  lazy val tests_e2 = e2.TestCases.add(tests_e1)
  lazy val history_e2 = evolution.E2.extend(history_e1)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e2, controllerAddress)
  override lazy val rep = new ExpressionDomain(history_e2, tests_e2) with ExpressionSynthesis with Structure
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e2)
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E2_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e3"
  // all tests are derived from the model.
  lazy val tests_e3 = e3.TestCases.add(tests_e2)
  lazy val history_e3 = evolution.E3.extend(history_e2)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e3, controllerAddress)
  override lazy val rep = new ExpressionDomain(history_e3, tests_e3) with ExpressionSynthesis with Structure
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e3)
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E3_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e4"
  // all tests are derived from the model.
  lazy val tests_e4 = e4.TestCases.add(tests_e3)
  lazy val history_e4 = evolution.E4.extend(history_e3)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e4, controllerAddress)
  override lazy val rep = new ExpressionDomain(history_e4, tests_e4) with ExpressionSynthesis with Structure
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e4)
}

//
//
//class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends E1_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {
//
//  override lazy val controllerAddress = "e2"
//
//  lazy val tests_e2:UnitSuite = e2.TestCases.add(tests_e1)
//
//  history.extend("e2", new DomainModel(
//    List.empty.asJava,
//    List[Operation](new PrettyP).asJava,
//  ))
//
//  override lazy val rep = new ExpressionDomain(history, tests_e2) with ExpressionSynthesis with e2.Model with e1.Model with e0.Model with InitializeRepository {}
//}
//
//
//class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends E2_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {
//
//  override lazy val controllerAddress = "e3"
//
//  val tests_e3:UnitSuite = e3.TestCases.add(tests_e2)
//
//  history.extend("e3", new DomainModel(
//    List[Exp](new Neg, new Mult, new Divd).asJava,
//    List.empty.asJava
//  ))
//
//  override lazy val rep = new ExpressionDomain(history, tests_e3) with ExpressionSynthesis with e3.Model with e2.Model with e1.Model with e0.Model with InitializeRepository {}
//}
//
//
//class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends E3_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {
//
//  override lazy val controllerAddress = "e4"
//
//  val tests_e4:UnitSuite = e4.TestCases.add(tests_e3)
//
//  history.extend("e4", new DomainModel(
//    List.empty.asJava,
//    List[Operation](new Collect, new SimplifyExpr).asJava
//  ))
//
//  override lazy val rep = new ExpressionDomain(history, tests_e4) with ExpressionSynthesis with e4.Model with e3.Model with e2.Model with e1.Model with e0.Model with InitializeRepository {}
//}
