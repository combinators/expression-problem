package example.expression.visitor

import javax.inject.Inject

import com.github.javaparser.ast.CompilationUnit
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.git.{EmptyResults, InhabitationController}
import org.combinators.templating.persistable.JavaPersistable._
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.operations._
import expression.{DomainModel, Exp, Operation}
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

import scala.collection.JavaConverters._

class Expression @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle) extends InhabitationController(webJars, applicationLifecycle) {

  // Configure the desired (sub)types and operations
  // no need to add 'Exp' to the model, since assumed always to be there
  // operations to have (including Eval).
  val first:DomainModel = new DomainModel(
    List[Exp](new Lit, new Add, new Sub, new Mult, new Divd).asJava,
    List[Operation](new Eval, new PrettyP).asJava
  )

  // Extension to domain model has new data variants and operations
  val other:DomainModel = new DomainModel(
    List[Exp](new Neg).asJava,
    List[Operation](new Collect, new SimplifyExpr).asJava
  )

  // demonstrate how to merge domain models with new capabilities
  // supported by POJO domain model
  val model = first.merge(other)

  lazy val repository = new ExpressionSynthesis(model) with Structure {}
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

  // Quickest way to request all targets
  var jobs = Gamma.InhabitationBatchJob[CompilationUnit](generated(generated.visitor))
      .addJob[CompilationUnit](exp(exp.base, new Exp))
      .addJob[CompilationUnit](ops(ops.visitor, new Eval))
      .addJob[CompilationUnit](exp(exp.visitor, new Lit))
      .addJob[CompilationUnit](exp(exp.visitor, new Add))
      .addJob[CompilationUnit](exp(exp.visitor, new Sub))
      .addJob[CompilationUnit](exp(exp.visitor, new Neg))
      .addJob[CompilationUnit](exp(exp.visitor, new Mult))
      .addJob[CompilationUnit](exp(exp.visitor, new Divd))
      .addJob[CompilationUnit](ops(ops.visitor, new PrettyP))
      .addJob[CompilationUnit](ops(ops.visitor, new Collect))
      .addJob[CompilationUnit](ops(ops.visitor, new SimplifyExpr))
      .addJob[CompilationUnit](driver)

  lazy val results = EmptyResults().addAll(jobs.run())
}
