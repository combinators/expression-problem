package example.expression.covariant

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import example.expression.ExpressionDomain
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.git._
import org.combinators.templating.persistable.JavaPersistable._
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.history.History
import expression.instances.UnitSuite
import expression.operations.SimplifyExpr
import expression.tests.AllTests
import expression.{DomainModel, Exp, Operation}
import org.combinators.cls.types.Constructor
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

import scala.collection.JavaConverters._

// https://bitbucket.org/yanlinwang/ep_trivially/src/7086d91a45c92c1522ec4d6f0618c574c2e2d562/JavaCode/EP/src/interfaceversion/InterfaceVersion.java?at=master&fileviewer=file-view-default

class Expression @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle) extends InhabitationController(webJars, applicationLifecycle) with RoutingEntries {

  val history:History = new History
  history.extend("e0", new DomainModel(
    List[Exp](new Lit, new Add).asJava,
    List[Operation](new Eval).asJava
  ))

  // evolution 1 (from Extensibility for the Masses example)
  history.extend("e1",  new DomainModel(
    List[Exp](new Sub).asJava,
    List.empty.asJava
  ))

  // evolution 2 (from Extensibility for the Masses example)
  history.extend("e2",  new DomainModel(
    List.empty.asJava,
    List[Operation](new PrettyP).asJava
  ))

  // Evolution 1: Extension to domain model has new data variants and operations
  history.extend("e3",  new DomainModel(
    List[Exp](new Neg, new Mult, new Divd).asJava,
    List.empty.asJava
  ))

  history.extend("e4",  new DomainModel(
    List.empty.asJava,
    List[Operation](new Collect, new SimplifyExpr).asJava
  ))

  // VISITOR solution has no choice but to merge all domain models.
 // val model:DomainModel = history.flatten

  // decide upon a set of test cases from which we can generate driver code/test cases.
  val allTests : UnitSuite =  new AllTests()

//  lazy val rep = new ExpressionDomain(history, tests_e0) with ExpressionSynthesis with e0.Model with InitializeRepository {}
//
//  lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), rep.domain)

  lazy val repository = new ExpressionDomain(history, allTests) with ExpressionSynthesis with Structure {}
  import repository._

  lazy val Gamma = repository.init(ReflectedRepository(repository, classLoader = this.getClass.getClassLoader), history)
//
//
//  lazy val repository = new ExpressionSynthesis(evolution_4,allTests) with Structure {}
//  import repository._
//
//  lazy val Gamma = repository.init(ReflectedRepository(repository, classLoader = this.getClass.getClassLoader), evolution_4)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

    val targets:Seq[Constructor] = Synthesizer.covariantTargets(history, "e4")
    lazy val results:Results =
      EmptyInhabitationBatchJobResults(Gamma).addJobs[CompilationUnit](targets).compute()

    lazy val controllerAddress: String = "ep"
}
