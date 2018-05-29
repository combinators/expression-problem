package example.expression.cpp

import java.nio.file.{Path, Paths}

import example.expression.{Base, ExpressionDomain}
import javax.inject.Inject
import org.combinators.templating.persistable.Persistable
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.git.{EmptyResults, InhabitationController}
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.history.History
import expression.instances.UnitSuite
import expression.operations.SimplifyExpr
import expression.{DomainModel, Exp, Operation}
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._
class Expression_CPP @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle) extends InhabitationController(webJars, applicationLifecycle) {

//  // Configure the desired (sub)types and operations
//  val model:DomainModel = new DomainModel()
//
//  // no need to add 'Exp' to the model, since assumed always to be there
//  model.data.add(new Lit)
//  model.data.add(new Add)
//  model.data.add(new Neg)
//  model.data.add(new Sub)
//
//  // operations to have (including Eval)
//  model.ops.add(new Eval)
//  model.ops.add(new PrettyP)
//  model.ops.add(new SimplifyExpr)
//  model.ops.add(new Collect)

  // Configure the desired (sub)types and operations
  // no need to add 'Exp' to the model, since assumed always to be there
  // operations to have (including Eval).
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
  def domain:DomainModel = history.flatten

  // decide upon a set of test cases from which we can generate driver code/test cases.
  val testCases : UnitSuite = new UnitSuite()

//  lazy val rep = new ExpressionDomain(history, tests_e0) with ExpressionSynthesis with e0.Model with InitializeRepository {}
//  lazy val Gamma = rep.init(ReflectedRepository(rep, classLoader = this.getClass.getClassLoader), rep.domain)

 // HACK: Will eliminate
  lazy val repository = new ExpressionDomain(history, testCases) with ExpressionSynthesis with Structure {
    def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod]] = {
      null
    }
  }
  import repository._

   lazy val Gamma = repository.init(ReflectedRepository(repository), history)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

  /**
    * Tell the framework to store stuff of type PythonWithPath at the location specified in Path.
    * The Path is relative to the Git repository.
    */
  implicit def PersistCPPFile: Persistable.Aux[CPPFile] = new Persistable {
    override def path(elem: CPPFile): Path = Paths.get(elem.fileName + ".cpp")
    override def rawText(elem: CPPFile): Array[Byte] = elem.toString.getBytes
    override type T = CPPFile
  }

  // produce concatenation of files in specific order for compilation purpose.
  // may prove challenging to have independent extensions...
  var jobs = Gamma.InhabitationBatchJob[CPPFile](module(module.base))
//
  lazy val results = EmptyResults().addAll(jobs.run())

}
