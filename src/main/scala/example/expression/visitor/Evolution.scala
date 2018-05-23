package example.expression.visitor


import com.github.javaparser.ast.CompilationUnit
import example.expression.ExpressionDomain
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

class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) with e0.Model {

  lazy val controllerAddress = "e0"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  override def history:History = evolution.E0.extend(super.history)
  override def testCases:UnitSuite = tests.e0.TestCases.add(super.testCases)

}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E0_Variation(web, app) with e0.Model with e1.Model {

  override lazy val controllerAddress = "e1"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  override def history:History = evolution.E1.extend(super.history)

  //def domain:DomainModel = history.flatten
  // all tests are derived from the model.
  override def testCases:UnitSuite = tests.e1.TestCases.add(super.testCases)
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E1_Variation(web, app) with e0.Model with e1.Model with e2.Model {

  override lazy val controllerAddress = "e2"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  override def history:History = evolution.E2.extend(super.history)

  //def domain:DomainModel = history.flatten
  // all tests are derived from the model.
  override def testCases:UnitSuite = tests.e2.TestCases.add(super.testCases)
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E2_Variation(web, app) with e0.Model with e1.Model with e2.Model with e3.Model {

  override lazy val controllerAddress = "e3"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  override def history:History = evolution.E3.extend(super.history)

  //def domain:DomainModel = history.flatten
  // all tests are derived from the model.
  override def testCases:UnitSuite = tests.e3.TestCases.add(super.testCases)
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E3_Variation(web, app) with e0.Model with e1.Model with e2.Model with e3.Model with e4.Model {

  override lazy val controllerAddress = "e4"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  override def history:History = evolution.E4.extend(super.history)

  //def domain:DomainModel = history.flatten
  // all tests are derived from the model.
  override def testCases:UnitSuite = tests.e4.TestCases.add(super.testCases)
}

// TODO: I bet these could be automagically constructed right from the "e1" .. "e4" in the URL for
// TODO: the variation (though the rep definition might be a challenge).
