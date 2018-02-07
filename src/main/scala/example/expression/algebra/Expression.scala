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

  val evolution_1:DomainModel = new DomainModel (
    List[Exp](new Sub).asJava,
    List.empty.asJava
  )

  // evolution 2 (from Extensibility for the Masses example)
  val evolution_2:DomainModel = new DomainModel(evolution_1,
    List.empty.asJava,
    List[Operation](new PrettyP).asJava
  )

  //model.data.add(new Neg)
  //model.data.add(new Sub)

  // operations to have (including Eval)
  //model.ops.add(new PrettyP)
  //model.ops.add(new SimplifyExpr)
  //model.ops.add(new Collect)

  lazy val repository = new ExpressionSynthesis(base) with Structure {}
  import repository._

  lazy val Gamma = repository.init(ReflectedRepository(repository, classLoader = this.getClass.getClassLoader), base)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  var jobs = Gamma.InhabitationBatchJob[CompilationUnit](ops(ops.base, new Eval))
      .addJob[CompilationUnit](exp(exp.base, new Exp))
      .addJob[CompilationUnit](ops(ops.algebra, new Eval))
     // .addJob[CompilationUnit](ops(ops.algebra, new PrettyP))

    // type interfaces (note: Exp is assumed above)

// add here as you are ready...
// Initial object algebra interface for expressions: integers and addition



lazy val results = EmptyResults().addAll(jobs.run())

}
