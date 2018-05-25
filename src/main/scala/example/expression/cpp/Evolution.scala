package example.expression.cpp

import java.nio.file.{Path, Paths}

import example.expression._
import expression.tests.AllTests
import shared.compilation.{CodeGenerationController, HasCodeGenerator}
import javax.inject.Inject
import org.combinators.templating.persistable.Persistable
import expression.history.History
import expression.instances.UnitSuite
import expression.DomainModel
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CPPFile](web, app)
  with ExpressionSynthesis with InstanceCodeGenerators with HasCodeGenerator {


  def history:History = new History
  def testCases:UnitSuite = new AllTests

  // for visitor, this brings all data/ops together into one place.
  override def domain:DomainModel = history.flatten

  // all targets are derived from the model
  def targets(hist:History, testCases:UnitSuite):Seq[CPPFile] = {
//
//    // need all subtypes from history for the visitor interface
//    val allSubTypes: Seq[Exp] = domain.data.asScala.foldLeft(Seq.empty[Exp]) {
//      case (combined, sub) => combined :+ sub
//    }
//
//    // combine specific targets
//    var tgts:Seq[CompilationUnit] = Seq.empty
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

    Seq.empty
  }

  /**
    * Tell the framework to store stuff of type PythonWithPath at the location specified in Path.
    * The Path is relative to the Git repository.
    */
  implicit def PersistCPPFile: Persistable.Aux[CPPFile] = new Persistable {
    override def path(elem: CPPFile): Path = Paths.get(elem.fileName + ".cpp")
    override def rawText(elem: CPPFile): Array[Byte] = elem.toString.getBytes
    override type T = CPPFile
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

  /**
    * Tell the framework to store stuff of type PythonWithPath at the location specified in Path.
    * The Path is relative to the Git repository.
    */
  override implicit def PersistCPPFile: Persistable.Aux[CPPFile] = new Persistable {
    override def path(elem: CPPFile): Path = Paths.get(elem.fileName + ".cpp")
    override def rawText(elem: CPPFile): Array[Byte] = elem.toString.getBytes
    override type T = CPPFile
  }
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E0_Variation(web, app) with e1.Model {

  override lazy val controllerAddress = "e1"

  override def history:History = evolution.E1.extend(super.history)
  override def testCases:UnitSuite = tests.e1.TestCases.add(super.testCases)
}
//
//class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends E1_Variation(web, app) with e2.Model {
//
//  override lazy val controllerAddress = "e2"
//
//  override def history:History = evolution.E2.extend(super.history)
//  override def testCases:UnitSuite = tests.e2.TestCases.add(super.testCases)
//}
//
//class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends E2_Variation(web, app) with e3.Model {
//
//  override lazy val controllerAddress = "e3"
//
//  override def history:History = evolution.E3.extend(super.history)
//  override def testCases:UnitSuite = tests.e3.TestCases.add(super.testCases)
//}
//
//class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends E3_Variation(web, app) with e4.Model {
//
//  override lazy val controllerAddress = "e4"
//
//  override def history:History = evolution.E4.extend(super.history)
//  override def testCases:UnitSuite = tests.e4.TestCases.add(super.testCases)
//}
