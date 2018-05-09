package example.expression.visitor

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import example.expression.visitor.tests.AllTests
import org.combinators.cls.interpreter.ReflectedRepository
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.instances.UnitSuite
import expression.operations._
import expression.{DomainModel, Exp, Operation}
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, InhabitationController, Results, RoutingEntries}
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.cls.types.Constructor
import scala.collection.JavaConverters._
import org.combinators.templating.persistable.JavaPersistable._

class Expression @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle) extends InhabitationController(webJars, applicationLifecycle) with RoutingEntries {

  // Configure the desired (sub)types and operations
  // no need to add 'Exp' to the model, since assumed always to be there
  // operations to have (including Eval).
  val base:DomainModel = new DomainModel(
    List[Exp](new Lit, new Add).asJava,
    List[Operation](new Eval).asJava
  )

  // evolution 1 (from Extensibility for the Masses example)
  val version_2:DomainModel = new DomainModel(base,
    List[Exp](new Sub).asJava,
    List.empty.asJava
  )

  // evolution 2 (from Extensibility for the Masses example)
  val version_3:DomainModel = new DomainModel(version_2,
    List.empty.asJava,
    List[Operation](new PrettyP).asJava
  )

  // Evolution 1: Extension to domain model has new data variants and operations
  val version_4:DomainModel = new DomainModel(version_3,
    List[Exp](new Neg, new Mult, new Divd).asJava,
    List.empty.asJava
  )

  val version_5:DomainModel = new DomainModel(version_4,
    List.empty.asJava,
    List[Operation](new Collect, new SimplifyExpr).asJava
  )

  // One extension is to add Boolean values and IFF operation. you can ADD/OR/XOR/NOR/
  // IFF(bool, sub-1, sub-2)
  // Simplify. Exponent (x^k where k is a double).  Sqrt(x^2) = x
  // What about detecting div-by-zero replace with a new LIT that is an exception
  //

  // demonstrate how to merge domain models with new capabilities
  // VISITOR solution has no choice but to merge all domain models.
  val model:DomainModel = version_5.flatten

  // decide upon a set of test cases from which we can generate driver code/test cases.
  val allTests : UnitSuite = new AllTests(model)

  lazy val repository = new ExpressionSynthesis(model, allTests)
  import repository._

  lazy val Gamma = {
    val base = ReflectedRepository(repository, classLoader = this.getClass.getClassLoader)
    val withExpressions =
      domain.data.asScala.foldLeft(base) {
        case (repo, sub) => repo.addCombinator(new BaseClass(sub)).addCombinator(new ImplClass(sub))
      }
    val withOps =
      domain.ops.asScala.foldLeft(withExpressions) {
        case (repo, op) => repo.addCombinator(new OpImpl(op))
      }

    withOps
  }

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  val targets:Seq[Constructor] = Synthesizer.visitorTargets(domain)
  lazy val results:Results =
    EmptyInhabitationBatchJobResults(Gamma).addJobs[CompilationUnit](targets).compute()

  lazy val controllerAddress: String = "expression"
}
