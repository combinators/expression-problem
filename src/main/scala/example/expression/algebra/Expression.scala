package example.expression.algebra

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import expression.data.{Add, Eval, Lit}
import expression.extensions.{Collect, Mult, Neg, PrettyP, Sub}
import expression.instances.UnitSuite
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
  val evolution_0:DomainModel = new DomainModel(
    List[Exp](new Lit, new Add).asJava,
    List[Operation](new Eval).asJava
  )

  val evolution_1:DomainModel = new DomainModel (evolution_0,
    List[Exp](new Sub).asJava,
    List.empty.asJava
  )

  // evolution 2 (from Extensibility for the Masses example)
  val evolution_2:DomainModel = new DomainModel(evolution_1,
    List.empty.asJava,
    List[Operation](new PrettyP).asJava
  )
  // Adding mult
  val evolution_3:DomainModel = new DomainModel(evolution_2,
    List[Exp](new Mult).asJava,
    List.empty.asJava
  )


  // decide upon a set of test cases from which we can generate driver code/test cases.
  val allTests : UnitSuite = new UnitSuite(evolution_3)

  // this is a hack, to pass in evol_2 since we won't be using it in futurte.
  lazy val repository = new ExpressionSynthesis(evolution_3, allTests) with Structure {}
  import repository._



  lazy val Gamma = {
    var base = ReflectedRepository(repository, classLoader = this.getClass.getClassLoader)

    // hack
    base = base
      .addCombinator(new OperationImpClass(evolution_2, new PrettyP,"Sub",domain_evolution.version2))//hacking

    base = domain.data.asScala.foldLeft(base){
      case (repo, sub) =>repo
        .addCombinator(new BaseClass(sub,"ExpAlg"))
    }

    base = base
      .addCombinator(new BaseExpClass(evolution_0, domain_evolution.version0))
      .addCombinator(new OperationBaseClass(evolution_0, new Eval()))

    // DEAL with BaseInterface for all operations in all domains
    evolution_0.ops.asScala.foreach {
        op:Operation => {
          base = base.addCombinator(new BaseInterface(op))
        }
      }
    evolution_2.ops.asScala.foreach {
      op:Operation => {
        base = base.addCombinator(new BaseInterface(op))
      }
    }

    base = base
      .addCombinator(new ExtendedInterface(evolution_1, "Sub", "ExpAlg<E>", domain_evolution.version1))
      .addCombinator(new OperationExtendedBaseClass(evolution_1, new Sub, new Eval, domain_evolution.version1))
      .addCombinator(new ExtendedInterface(evolution_3, "Mult", "ExpAlg<E>", domain_evolution.version3))
      .addCombinator(new OperationExtendedBaseClass(evolution_3, new Mult, new Eval, domain_evolution.version3))
      .addCombinator(new OperationSpecialExtendedBaseClass(evolution_3, new Mult, new PrettyP, domain_evolution.version3))
       //problem here need to fix OperationExtendedBaseClass
      .addCombinator(new OperationSpecialImpClass(evolution_3, new PrettyP,"ExpAlg", "Mult", domain_evolution.version2))//hacking
      .addCombinator(new OperationSpecialImpClass(evolution_3, new Eval,"SubExpAlg", "Mult", domain_evolution.version2))//hacking

    base
  }
  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents
  var jobs = Gamma.InhabitationBatchJob[CompilationUnit](domain_evolution(domain_evolution.baseClass, domain_evolution.version0))
      .addJob[CompilationUnit](ops (ops.baseInterface, new Eval))
      .addJob[CompilationUnit](ops (ops.baseInterface, new PrettyP))
      .addJob[CompilationUnit](ops (ops.baseClass,new Eval))
      .addJob[CompilationUnit](domain_evolution(domain_evolution.extendedInterface , domain_evolution.version1))
      .addJob[CompilationUnit](domain_evolution(domain_evolution.extendedData, domain_evolution.version1))
      .addJob[CompilationUnit](domain_evolution(domain_evolution.extendedOp, domain_evolution.version2))
    .addJob[CompilationUnit](domain_evolution(domain_evolution.extendedInterface , domain_evolution.version3))
    //.addJob[CompilationUnit](domain_evolution(domain_evolution.extendedData, domain_evolution.version3))
    .addJob[CompilationUnit](evolved2_ops(evolved2_ops.base, new PrettyP, "ExpAlg", "Mult"))  // hack.
    .addJob[CompilationUnit](evolved2_ops(evolved2_ops.base, new Eval, "SubExpAlg", "Mult"))  // hack.
      .addJob[CompilationUnit](driver)
  // domain_evolution(domain_evolution.extendedData, vers)

  //(ops(ops.baseInterface, new Eval))
//    .addJob[CompilationUnit](ops(ops.baseClass, new Eval))
//    .addJob[CompilationUnit](domain_evolution(domain_evolution.version0))
//
//    .addJob[CompilationUnit](exp(exp.base, new Lit))
//    .addJob[CompilationUnit](exp(exp.base, new Add))
//      .addJob[CompilationUnit](ops(ops.base, new Eval))
//      .addJob[CompilationUnit](ops(ops.base, new PrettyP))
//      .addJob[CompilationUnit](evolved_exp(exp.base, new Sub, "ExpAlg"))
//      .addJob[CompilationUnit](evolved_ops (ops.algebra, new Eval, new Sub, "EvalExpAlg"))
//      .addJob[CompilationUnit](evolved2_ops (ops.algebra, new PrettyP,"SubExpAlg"))



    // type interfaces (note: Exp is assumed above)

// add here as you are ready...
// Initial object algebra interface for expressions: integers and addition



lazy val results = EmptyResults().addAll(jobs.run())

}
