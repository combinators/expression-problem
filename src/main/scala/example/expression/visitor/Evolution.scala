package example.expression.visitor

import java.nio.file.{Path, Paths}

import com.github.javaparser.ast.CompilationUnit
import example.expression.ExpressionDomain
import expression.history.History
import expression.instances.UnitSuite
import expression.tests.AllTests
import javax.inject.Inject
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.{ReflectedRepository, combinator}
import org.combinators.cls.types.Constructor
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.templating.persistable.JavaPersistable._
import example.expression._
import expression.data.{Add, Eval, Lit}
import expression.extensions.Sub
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.persistable.Persistable

abstract class Foundation (web: WebJarsUtil, app: ApplicationLifecycle)
  extends InhabitationController(web, app) with RoutingEntries {

  // to inherit from class overwrite comp
  val comps:Seq[CompilationUnit]

  class solutionRepository {
    @combinator object Solution {
      def apply():Unit = { }

      val semanticType:Type = 'Solution
    }
  }

  lazy val Gamma = ReflectedRepository(new solutionRepository, classLoader = this.getClass.getClassLoader)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  /**
    * Tell the framework to store stuff of type PythonWithPath at the location specified in Path.
    * The Path is relative to the Git repository.
    */
  implicit def PersistDummy: Persistable.Aux[Unit] = new Persistable {
    override def path(elem: Unit): Path = Paths.get("solutions.txt")
    override def rawText(elem: Unit): Array[Byte] = elem.toString.getBytes
    override type T = Unit
  }

  /** Has to be lazy so subclasses can compute model. */
  lazy val results:Results = comps.foldLeft(EmptyInhabitationBatchJobResults(Gamma)
    .addJob[Unit]()
    .compute())((result, comp) => result.addExternalArtifact[CompilationUnit](comp))
}

class Last @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  lazy val history_e0:History = evolution.E0.extend(new History)

  // all tests are derived from the model.
  lazy val tests_e0 = tests.e0.TestCases.add(new AllTests())
  lazy val rep = new ExpressionDomain(history_e0, tests_e0) with ExpressionSynthesis with e0.Model with InitializeRepository {}

  val comps:Seq[CompilationUnit] =  Seq(rep.Visitor.apply(), new rep.OpImpl(List(new Add, new Lit), new Eval).apply)

  lazy val controllerAddress = "e0"
  override val routingPrefix: Option[String] = Some("dummy")

  // forces the generation of the first (only) instance
  //prepare(0)

}

class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends InhabitationController(web, app) with RoutingEntries {

  lazy val controllerAddress = "e0"

  // start off the domain Model. Subsequent subclasses merge their domain models with ours, to satisfy the
  // demands of the visitor solution to the expression problem.
  lazy val history_e0:History = evolution.E0.extend(new History)

  // all tests are derived from the model.
  lazy val tests_e0 = tests.e0.TestCases.add(new AllTests())

  lazy val rep = new ExpressionDomain(history_e0, tests_e0) with ExpressionSynthesis with e0.Model with InitializeRepository {}

  lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e0)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

//  /** Has to be lazy so subclasses can compute model. */
  lazy val targets:Seq[Constructor] = Synthesizer.visitorTargets(rep.domain)
  lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma).addJobs[CompilationUnit](targets)
      .compute()

  // all accomplished within the 'visitor' family
  override val routingPrefix: Option[String] = Some("visitor")
}

// TODO: I bet these could be automagically constructed right from the "e1" .. "e4" in the URL for
// TODO: the variation (though the rep definition might be a challenge).

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E0_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e1"

  lazy val tests_e1:UnitSuite = tests.e1.TestCases.add(tests_e0)

  lazy val history_e1:History = evolution.E1.extend(history_e0)


  override lazy val rep = new ExpressionDomain(history_e1, tests_e1) with ExpressionSynthesis with e1.Model with e0.Model with InitializeRepository {}
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e1)
}



class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E1_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e2"

  lazy val tests_e2:UnitSuite = tests.e2.TestCases.add(tests_e1)
  lazy val history_e2:History = evolution.E2.extend(history_e1)


  override lazy val rep = new ExpressionDomain(history_e2, tests_e2) with ExpressionSynthesis with e2.Model with e1.Model with e0.Model with InitializeRepository {}
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e2)
}


class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E2_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e3"

  lazy val tests_e3:UnitSuite = tests.e3.TestCases.add(tests_e2)
  lazy val history_e3:History = evolution.E3.extend(history_e2)


  override lazy val rep = new ExpressionDomain(history_e3, tests_e3) with ExpressionSynthesis with e3.Model with e2.Model with e1.Model with e0.Model with InitializeRepository {}
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e3)
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends E3_Variation (web: WebJarsUtil, app: ApplicationLifecycle) {

  override lazy val controllerAddress = "e4"

  lazy val tests_e4:UnitSuite = tests.e4.TestCases.add(tests_e3)
  lazy val history_e4:History = evolution.E4.extend(history_e3)


  override lazy val rep = new ExpressionDomain(history_e4, tests_e4) with ExpressionSynthesis with e4.Model with e3.Model with e2.Model with e1.Model with e0.Model with InitializeRepository {}
  override lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), history_e4)

}
