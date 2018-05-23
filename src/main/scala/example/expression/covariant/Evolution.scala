package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import example.expression.ExpressionDomain
import example.expression._
import expression.{DomainModel, Exp}
import expression.history.History
import expression.instances.UnitSuite
import expression.tests.AllTests
import javax.inject.Inject
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.{CodeGenerationController, HasCodeGenerator}

import scala.collection.JavaConverters._

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
    with ExpressionSynthesis with InstanceCodeGenerators with HasCodeGenerator {

  def history:History = new History
  def testCases:UnitSuite = new AllTests

  // for visitor, this brings all data/ops together into one place.
  lazy val domain:DomainModel = history.flatten

  // all targets are derived from the model
  def targets(hist:History, testCases:UnitSuite):Seq[CompilationUnit] = {

    // need all subtypes from history for the visitor interface
    val allSubTypes: Seq[Exp] = domain.data.asScala.foldLeft(Seq.empty[Exp]) {
      case (combined, sub) => combined :+ sub
    }

    // combine specific targets
    var tgts:Seq[CompilationUnit] = Seq.empty
//    hist.asScala.foreach(domain =>
//      domain.data.asScala.foreach(exp =>
//        tgts = tgts :+ ImplClass(exp, BaseClass(exp))
//      ))
//
//    hist.asScala.foreach(domain =>
//      domain.ops.asScala.foreach(op =>
//        tgts = tgts :+ OpImpl(allSubTypes, op, codeGenerator)
//      ))
//
//    tgts = tgts :+ Visitor(domain)
//    tgts = tgts :+ BaseExpClass
//    tgts = tgts :+ Driver(defaultInstance.instanceGenerators, testCases)

    tgts
  }

  override lazy val generatedCode = targets(history, testCases)

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("visitor")
}

class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends InhabitationController(web, app) with RoutingEntries {

  lazy val controllerAddress = "e0"

  // all tests are derived from the model.
  lazy val tests_e0 = tests.e0.TestCases.add(new AllTests())
  lazy val history_e0 = evolution.E0.extend(new History)

  lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e0, controllerAddress)
  lazy val rep_e0 = new ExpressionDomain(history_e0, tests_e0) with ExpressionSynthesis with Structure with Registry with e0.Model
  lazy val Gamma_e0 = rep_e0.init(ReflectedRepository(rep_e0, classLoader = this.getClass.getClassLoader), history_e0)


  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma_e0.combinatorComponents

  /** Has to be lazy so subclasses can compute model. */
  lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e0).addJobs[CompilationUnit](targets).compute()

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("covariant")
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E0_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e1"
  // all tests are derived from the model.
  lazy val tests_e1 = tests.e1.TestCases.add(tests_e0)
  lazy val history_e1 = evolution.E1.extend(history_e0)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e1, controllerAddress)
  lazy val rep_e1 = new ExpressionDomain(history_e1, tests_e1) with ExpressionSynthesis with Structure with Registry with e0.Model with e1.Model
  lazy val Gamma_e1 = rep_e1.init(ReflectedRepository(rep_e1, classLoader = this.getClass.getClassLoader), history_e1)

  /** This needs to be defined, and it is set from Gamma. */
  override lazy val combinatorComponents = Gamma_e1.combinatorComponents
  override lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e1).addJobs[CompilationUnit](targets).compute()
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E1_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e2"
  // all tests are derived from the model.
  lazy val tests_e2 = tests.e2.TestCases.add(tests_e1)
  lazy val history_e2 = evolution.E2.extend(history_e1)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e2, controllerAddress)
  lazy val rep_e2 = new ExpressionDomain(history_e2, tests_e2) with ExpressionSynthesis with Structure with Registry with e0.Model with e1.Model with e2.Model
  lazy val Gamma_e2 = rep_e2.init(ReflectedRepository(rep_e2, classLoader = this.getClass.getClassLoader), history_e2)
  override lazy val combinatorComponents = Gamma_e2.combinatorComponents
  override lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e2).addJobs[CompilationUnit](targets).compute()
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E2_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e3"
  // all tests are derived from the model.
  lazy val tests_e3 = tests.e3.TestCases.add(tests_e2)
  lazy val history_e3 = evolution.E3.extend(history_e2)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e3, controllerAddress)
  lazy val rep_e3 = new ExpressionDomain(history_e3, tests_e3) with ExpressionSynthesis with Structure with Registry with e0.Model with e1.Model with e2.Model with e3.Model
  lazy val Gamma_e3 = rep_e3.init(ReflectedRepository(rep_e3, classLoader = this.getClass.getClassLoader), history_e3)
  override lazy val combinatorComponents = Gamma_e3.combinatorComponents
  override lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e3).addJobs[CompilationUnit](targets).compute()
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E3_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e4"
  // all tests are derived from the model.
  lazy val tests_e4 = tests.e4.TestCases.add(tests_e3)
  lazy val history_e4 = evolution.E4.extend(history_e3)

  override lazy val targets:Seq[Constructor] = Synthesizer.covariantTargets(history_e4, controllerAddress)
  lazy val rep_e4 = new ExpressionDomain(history_e4, tests_e4) with ExpressionSynthesis with Structure  with Registry with e0.Model with e1.Model with e2.Model with e3.Model with e4.Model
  lazy val Gamma_e4 = rep_e4.init(ReflectedRepository(rep_e4, classLoader = this.getClass.getClassLoader), history_e4)
  override lazy val combinatorComponents = Gamma_e4.combinatorComponents
  override lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma_e4).addJobs[CompilationUnit](targets).compute()
}
