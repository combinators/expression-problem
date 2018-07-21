package example.expression.cpp

import example.expression._
import expression.tests.AllTests
import shared.compilation.CodeGenerationController
import javax.inject.Inject
import expression.history.History
import expression.instances.UnitSuite
import expression.{Exp, Operation}
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import CPPFileUtils._

import scala.collection.JavaConverters._

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CPPFile](web, app)
    with ExpressionSynthesis with Structure with InstanceCodeGenerators with HasCPPCodeGenerator {

  def history:History = new History
  def testCases:UnitSuite = new AllTests

  // all targets are derived from the model
  def targets(hist:History, testCases:UnitSuite):Seq[CPPFile] = {
    var tgts:Seq[CPPFile] = Seq.empty

    tgts = tgts :+ StandardHeaderFile
    tgts = tgts :+ Visitor
    tgts = tgts :+ BaseExpClass

    hist.asScala.foreach (domain =>
      domain.ops.asScala.foreach {
        op:Operation => {
          tgts = tgts :+ OpDecl(op)
        }
      }
    )

    // need all subtypes from history for the visitor interface
    var allSubTypes:Seq[Exp] = Seq.empty

    hist.iterator.asScala.foreach(domain =>
      domain.data.asScala.foreach {
        exp:Exp => {
          allSubTypes = allSubTypes :+ exp
        }
      })

    // combine specific targets
    hist.asScala.foreach(domain =>
      domain.data.asScala.foreach(exp => {
        //tgts = tgts :+ BaseClass(exp)
        tgts = tgts :+ ExpClassDecl(exp)
        tgts = tgts :+ ExpImplClass(exp)
      }
      ))

      tgts = tgts :+ Driver
      tgts = tgts :+ TestSuite(defaultInstance.instanceGenerators, testCases)
    tgts
  }
  override lazy val generatedCode = targets(history, testCases)

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("cpp")
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) with e0.Model with cpputest.e0.Test {

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
  extends M1_Variation(web, app) with e2.Model with cpputest.e2.Test {

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


//class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends M3_Variation(web, app) with e4.Model {
//
//  override lazy val controllerAddress = "m4"
//
//  override def history:History = evolution.J4.extend(super.history)
//  override def testCases:UnitSuite = tests.e4.TestCases.add(super.testCases)
//}
