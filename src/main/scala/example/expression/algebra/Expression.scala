package example.expression.algebra

import javax.inject.Inject

import com.github.javaparser.ast.CompilationUnit
import expression.data.{Add, Eval, Lit}
import expression.extensions.{Collect, Neg, PrettyP, Sub}
import expression.{DomainModel, Exp, Operation}
import org.combinators.cls.git.{EmptyResults, InhabitationController}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

// https://bitbucket.org/yanlinwang/ep_trivially/src/7086d91a45c92c1522ec4d6f0618c574c2e2d562/JavaCode/EP/src/interfaceversion/InterfaceVersion.java?at=master&fileviewer=file-view-default

import scala.collection.JavaConverters._    // Needed for asJava

class Expression @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle) extends InhabitationController(webJars, applicationLifecycle) {

  // Configure the desired (sub)types and operations
  val base:DomainModel = new DomainModel(
    List[Exp](new Lit, new Add).asJava,
    List[Operation](new Eval).asJava
  )

  val evolution_1:DomainModel = new DomainModel ( /// base,
    List[Exp](new Sub).asJava,
    List.empty.asJava
  )

  // evolution 2 (from Extensibility for the Masses example)
  val evolution_2:DomainModel = new DomainModel(evolution_1,
    List.empty.asJava,
    List[Operation](new PrettyP).asJava
  )

  val model:DomainModel = evolution_2.flatten

  //model.data.add(new Neg)
  //model.data.add(new Sub)

  // operations to have (including Eval)
  //model.ops.add(new PrettyP)
  //model.ops.add(new SimplifyExpr)
  //model.ops.add(new Collect)

  lazy val repository = new ExpressionSynthesis(model) with Structure {}
  import repository._

//  lazy val Gamma = repository.init(ReflectedRepository(repository, classLoader = this.getClass.getClassLoader), base)

  lazy val Gamma = {
    val base = ReflectedRepository(repository, classLoader = this.getClass.getClassLoader)

    // also will add 'withExpressions' as was done in the visitor package
    val withOps =
      domain.ops.asScala.foldLeft(base) {
        case (repo, op) => repo
            .addCombinator(new OperationBaseClass(op))
            .addCombinator(new OpImpl(op))
      }

    // any exp-specific combinators are added here...
//    val withVariants =
//      domain.data.asScala.foldLeft(withOps) {
//        case (repo, exp) => repo.addCombinator (something)
//      }

    // can't forget to add the base Eval for the entire problem space
    val addBase = withOps.addCombinator(new BaseInterface(new Eval()))

    // this should be derived from domain model evolution trace. For now hack in.
    addBase
      .addCombinator(new BaseClass(new Sub, "ExpAlg"))
      .addCombinator(new OperationExtendedBaseClass(new Eval, new Sub, "EvalExpAlg"))
  }

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  var jobs = Gamma.InhabitationBatchJob[CompilationUnit](ops(ops.base, new Eval))
      .addJob[CompilationUnit](exp(exp.base, new Exp))
      .addJob[CompilationUnit](ops(ops.algebra, new Eval))
      .addJob[CompilationUnit](ops(ops.algebra, new PrettyP))
      .addJob[CompilationUnit](evolved_exp(exp.base, new Sub, "ExpAlg"))
      .addJob[CompilationUnit](evolved_ops (ops.algebra, new Eval, new Sub, "EvalExpAlg"))


    // type interfaces (note: Exp is assumed above)

// add here as you are ready...
// Initial object algebra interface for expressions: integers and addition



lazy val results = EmptyResults().addAll(jobs.run())

}
