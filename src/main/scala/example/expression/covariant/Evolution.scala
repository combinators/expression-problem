package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.stmt.Statement
import example.expression._
import expression.data.Eval
import expression.{DomainModel, Exp, FunctionMethod, Operation}
import expression.history.History
import expression.instances.UnitSuite
import expression.tests.AllTests
import javax.inject.Inject
import org.combinators.cls.git.RoutingEntries
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.{CodeGenerationController, HasCodeGenerator}

import scala.collection.JavaConverters._

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
    with ExpressionSynthesis with Structure with Registry with HasCodeGenerator {

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

    //  registerImpl(history, new Eval, new FunctionMethod("eval", Types.Double)).foreach(comb =>
    // every operation gets registered
    val flat = history.flatten
    val op = new Eval
    val fm = new FunctionMethod(op.name, op.`type`)
    flat.data.asScala.foreach(exp => {
      val comb: Seq[Statement] = codeGenerator(new Eval).get(exp).get
      val unit:CompilationUnit = SubInterface(exp)

      tgts = tgts :+ AddDefaultImpl(fm, exp, comb, unit)
    })

    tgts = tgts :+ Driver(testCases)
    tgts = tgts :+ BaseExpInterface

    // strip out Eval from this list?
    val subsets:List[List[Operation]] = flat.ops.asScala.toSet[Operation].filterNot(p => p.getClass.getSimpleName.equals("Eval")).subsets.map(_.toList).toList.filter(_.nonEmpty)
    //
    subsets.foreach {
      sub: List[Operation] => {
        val sorted = sub.sortWith(_.getClass.getSimpleName < _.getClass.getSimpleName)

        if (sorted.length == 1) {
        tgts = tgts :+ AddOperation(sorted.head)    // ep(ep.interface, sub.head)

        } else {
          // only call for > 1 subset length
          tgts = tgts :+ AddMultiOperationInterface(sorted)   // ep(ep.interface, sub)
          flat.data.asScala.foreach(
            e => {
              tgts = tgts :+ AddMultiOperation(sorted, e)   // ep(ep.interface, e, sub)
              tgts = tgts :+ FinalMultiClass(sorted, e)    //     ep(ep.finalType, e, sub)
            }
          )
        }
      }
    }

    //
    // every sub-type gets a target
    flat.data.asScala.foreach (e =>
      tgts = tgts :+ FinalClass(e)    //seq = seq :+ ep(ep.finalType, e)
    )

    //
    //    // default methods for Eval [ALREADY DONE ABOVE]
    //    flat.data.asScala.foreach(
    //      e => seq = seq :+ ep(ep.defaultMethods, e, new Eval)
    //    )
    //
    // every type x op gets a target
    flat.data.asScala.foreach (
      e => {
        // Skip all Eval...
        flat.ops.asScala.filterNot(p => p.getClass.getSimpleName.equals("Eval")).foreach (
          o => {
            //seq = seq :+ ep(ep.finalType, e, List(o))
            tgts = tgts :+ FinalMultiClass(List(o), e)

            //seq = seq :+ ep(ep.interface, e, o)
            val comb: Option[Seq[Statement]] = codeGenerator(o).get(e)
            if (comb.isDefined) {
              tgts = tgts :+ AddExpOperation(e, o, comb.get)
            } else {
              println (">>>> No Exp found for" + e.getClass.getSimpleName)
            }
          }
        )
      }
    )
    //
    tgts
  }

  override lazy val generatedCode = targets(history, testCases)

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("covariant")
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) with RoutingEntries with e0.Model {

  lazy val controllerAddress = "m0"

  // all tests are derived from the model.
  override def history:History = evolution.J0.extend(super.history)
  override def testCases:UnitSuite = tests.e0.TestCases.add(super.testCases)
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M0_Variation (web: WebJarsUtil, app: ApplicationLifecycle) with e1.Model {

  override lazy val controllerAddress = "m1"

  override def history:History = evolution.J1.extend(super.history)
  override def testCases:UnitSuite = tests.e1.TestCases.add(super.testCases)
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M1_Variation (web: WebJarsUtil, app: ApplicationLifecycle) with e2.Model {

  override lazy val controllerAddress = "m2"

  override def history:History = evolution.J2.extend(super.history)
  override def testCases:UnitSuite = tests.e2.TestCases.add(super.testCases)
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M2_Variation (web: WebJarsUtil, app: ApplicationLifecycle) with e3.Model {

  override lazy val controllerAddress = "m3"

  override def history:History = evolution.J3.extend(super.history)
  override def testCases:UnitSuite = tests.e3.TestCases.add(super.testCases)
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends M3_Variation (web: WebJarsUtil, app: ApplicationLifecycle) with e4.Model {

  override lazy val controllerAddress = "m4"

  override def history:History = evolution.J4.extend(super.history)
  override def testCases:UnitSuite = tests.e4.TestCases.add(super.testCases)
}

