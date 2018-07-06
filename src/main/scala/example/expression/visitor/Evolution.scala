package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import expression.history.History
import expression.instances.UnitSuite
import expression.tests.AllTests
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.templating.persistable.JavaPersistable._
import example.expression._
import expression.{DomainModel, Exp}
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
    hist.asScala.foreach(domain =>
      domain.data.asScala.foreach(exp =>
        tgts = tgts :+ ImplClass(exp, BaseClass(exp))
      ))

    hist.asScala.foreach(domain =>
      domain.ops.asScala.foreach(op =>
        tgts = tgts :+ OpImpl(allSubTypes, op, codeGenerator)
      ))

    tgts = tgts :+ Visitor(domain)
    tgts = tgts :+ BaseExpClass
    tgts = tgts :+ Driver(defaultInstance.instanceGenerators, testCases)

    tgts
  }

  override lazy val generatedCode = targets(history, testCases)

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("visitor")
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) with e0.Model {

  lazy val controllerAddress = "m0"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  override def history:History = evolution.J0.extend(super.history)
  override def testCases:UnitSuite = tests.e0.TestCases.add(super.testCases)

}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M0_Variation(web, app) with e1.Model {

  override lazy val controllerAddress = "m1"

  override def history:History = evolution.J1.extend(super.history)
  override def testCases:UnitSuite = tests.e1.TestCases.add(super.testCases)
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M1_Variation(web, app) with e2.Model {

  override lazy val controllerAddress = "m2"

  override def history:History = evolution.J2.extend(super.history)
  override def testCases:UnitSuite = tests.e2.TestCases.add(super.testCases)
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M2_Variation(web, app) with e3.Model {

  override lazy val controllerAddress = "m3"

  override def history:History = evolution.J3.extend(super.history)
  override def testCases:UnitSuite = tests.e3.TestCases.add(super.testCases)
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M3_Variation(web, app) with e4.Model {

  override lazy val controllerAddress = "m4"

  override def history:History = evolution.J4.extend(super.history)
  override def testCases:UnitSuite = tests.e4.TestCases.add(super.testCases)
}
