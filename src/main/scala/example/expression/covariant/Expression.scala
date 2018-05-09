package example.expression.covariant

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import example.expression.covariant.tests.AllTests
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.git._
import org.combinators.templating.persistable.JavaPersistable._
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.instances.UnitSuite
import expression.operations.SimplifyExpr
import expression.{DomainModel, Exp, Operation}
import org.combinators.cls.types.Constructor
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

import scala.collection.JavaConverters._

// https://bitbucket.org/yanlinwang/ep_trivially/src/7086d91a45c92c1522ec4d6f0618c574c2e2d562/JavaCode/EP/src/interfaceversion/InterfaceVersion.java?at=master&fileviewer=file-view-default

class Expression @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle) extends InhabitationController(webJars, applicationLifecycle) with RoutingEntries {

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
    List[Exp](new Mult, new Neg, new Divd).asJava,
    List.empty.asJava
  )

  val evolution_4:DomainModel = new DomainModel(evolution_3,
    List.empty.asJava,
    List[Operation](new Collect, new SimplifyExpr).asJava
  )

  // decide upon a set of test cases from which we can generate driver code/test cases.
  val allTests : UnitSuite =  new AllTests(evolution_4)

  lazy val repository = new ExpressionSynthesis(evolution_4,allTests) with Structure {}
  import repository._

  lazy val Gamma = repository.init(ReflectedRepository(repository, classLoader = this.getClass.getClassLoader), evolution_4)

  /** This needs to be defined, and it is set from Gamma. */
  lazy val combinatorComponents = Gamma.combinatorComponents

    val targets:Seq[Constructor] = Synthesizer.covariantTargets(domain)
    lazy val results:Results =
      EmptyInhabitationBatchJobResults(Gamma).addJobs[CompilationUnit](targets).compute()

    lazy val controllerAddress: String = "ep"


//
//  var jobs = Gamma.InhabitationBatchJob[CompilationUnit](ep(ep.interface, new Exp))
//    // type interfaces (note: Exp is assumed above)
//    .addJob[CompilationUnit](ep(ep.interface, new PrettyP))
//    .addJob[CompilationUnit](ep(ep.interface, new Collect))
//    //.addJob[CompilationUnit](ep(ep.interface, new SimplifyExpr))
//
//    .addJob[CompilationUnit](driver)
//    .addJob[CompilationUnit](ep(ep.finalType, new Lit))
//    .addJob[CompilationUnit](ep(ep.finalType, new Add))
//    .addJob[CompilationUnit](ep(ep.finalType, new Sub))
//    .addJob[CompilationUnit](ep(ep.finalType, new Mult))
//    .addJob[CompilationUnit](ep(ep.finalType, new Divd))
//    .addJob[CompilationUnit](ep(ep.finalType, new Neg))
//
//    // default
//    .addJob[CompilationUnit](ep(ep.defaultMethods, new Lit, new Eval))
//    .addJob[CompilationUnit](ep(ep.defaultMethods, new Add, new Eval))
//    .addJob[CompilationUnit](ep(ep.defaultMethods, new Sub, new Eval))
//    .addJob[CompilationUnit](ep(ep.defaultMethods, new Mult, new Eval))
//    .addJob[CompilationUnit](ep(ep.defaultMethods, new Divd, new Eval))
//    .addJob[CompilationUnit](ep(ep.defaultMethods, new Neg, new Eval))
//
//    //.addJob[CompilationUnit](ep(ep.interface, new SimplifyExpr))
//    .addJob[CompilationUnit](ep(ep.interface, new Lit, new PrettyP))
//    .addJob[CompilationUnit](ep(ep.interface, new Add, new PrettyP))
//    .addJob[CompilationUnit](ep(ep.interface, new Sub, new PrettyP))
//    .addJob[CompilationUnit](ep(ep.interface, new Mult, new PrettyP))
//    .addJob[CompilationUnit](ep(ep.interface, new Divd, new PrettyP))
//    .addJob[CompilationUnit](ep(ep.interface, new Neg, new PrettyP))
//
//    .addJob[CompilationUnit](ep(ep.interface, new Lit, new Collect))
//    .addJob[CompilationUnit](ep(ep.interface, new Add, new Collect))
//    .addJob[CompilationUnit](ep(ep.interface, new Sub, new Collect))
//    .addJob[CompilationUnit](ep(ep.interface, new Mult, new Collect))
//    .addJob[CompilationUnit](ep(ep.interface, new Divd, new Collect))
//    .addJob[CompilationUnit](ep(ep.interface, new Neg, new Collect))
//
//    // potentially a bad tuple size...
//    .addJob[CompilationUnit](ep(ep.interface, List(new PrettyP, new Collect)))
//
////    .addJob[CompilationUnit](ep(ep.interface, new Lit, new SimplifyExpr))
////    .addJob[CompilationUnit](ep(ep.interface, new Add, new SimplifyExpr))
////    .addJob[CompilationUnit](ep(ep.interface, new Sub, new SimplifyExpr))
////    .addJob[CompilationUnit](ep(ep.interface, new Mult, new SimplifyExpr))
////    .addJob[CompilationUnit](ep(ep.interface, new Divd, new SimplifyExpr))
////    .addJob[CompilationUnit](ep(ep.interface, new Neg, new SimplifyExpr))
////
////    .addJob[CompilationUnit](ep(ep.interface, new Lit, List(new SimplifyExpr, new Collect)))
////    .addJob[CompilationUnit](ep(ep.interface, new Add, List(new SimplifyExpr, new Collect)))
////    .addJob[CompilationUnit](ep(ep.interface, new Sub, List(new SimplifyExpr, new Collect)))
////    .addJob[CompilationUnit](ep(ep.interface, new Mult, List(new SimplifyExpr, new Collect)))
////    .addJob[CompilationUnit](ep(ep.interface, new Divd, List(new SimplifyExpr, new Collect)))
////    .addJob[CompilationUnit](ep(ep.interface, new Neg, List(new SimplifyExpr, new Collect)))
//
//    .addJob[CompilationUnit](ep(ep.interface, new Lit, List(new PrettyP, new Collect)))
//    .addJob[CompilationUnit](ep(ep.interface, new Add, List(new PrettyP, new Collect)))
//    .addJob[CompilationUnit](ep(ep.interface, new Sub, List(new PrettyP, new Collect)))
//    .addJob[CompilationUnit](ep(ep.interface, new Mult, List(new PrettyP, new Collect)))
//    .addJob[CompilationUnit](ep(ep.interface, new Divd, List(new PrettyP, new Collect)))
//    .addJob[CompilationUnit](ep(ep.interface, new Neg, List(new PrettyP, new Collect)))
//
////    .addJob[CompilationUnit](ep(ep.interface, new Lit, List(new SimplifyExpr, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.interface, new Add, List(new SimplifyExpr, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.interface, new Sub, List(new SimplifyExpr, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.interface, new Mult, List(new SimplifyExpr, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.interface, new Divd, List(new SimplifyExpr, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.interface, new Neg, List(new SimplifyExpr, new PrettyP)))
//
//
//    //    .addJob[CompilationUnit](ep(ep.interface, new Lit, List(new Collect, new Eval, new PrettyP)))  // PP must come after C
////    .addJob[CompilationUnit](ep(ep.interface, new Add, List(new Collect, new Eval, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.interface, new Sub, List(new Collect, new Eval, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.interface, new Neg, List(new Collect, new Eval, new PrettyP)))
//
//    //.addJob[CompilationUnit](ep(ep.interface, List(new Collect, new PrettyP, new SimplifyExpr)))
//
//    .addJob[CompilationUnit](ep(ep.finalType, new Lit, List(new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Add, List(new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Sub, List(new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Mult, List(new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Divd, List(new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Neg, List(new PrettyP)))
//
//    .addJob[CompilationUnit](ep(ep.finalType, new Lit, List(new Collect)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Add, List(new Collect)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Sub, List(new Collect)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Mult, List(new Collect)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Divd, List(new Collect)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Neg, List(new Collect)))
//
//    .addJob[CompilationUnit](ep(ep.finalType, new Lit, List(new Collect, new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Add, List(new Collect, new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Sub, List(new Collect, new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Mult, List(new Collect, new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Divd, List(new Collect, new PrettyP)))
//    .addJob[CompilationUnit](ep(ep.finalType, new Neg, List(new Collect, new PrettyP)))
//
////    .addJob[CompilationUnit](ep(ep.finalType, new Lit, List(new Collect, new Eval, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.finalType, new Add, List(new Collect, new Eval, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.finalType, new Sub, List(new Collect, new Eval, new PrettyP)))
////    .addJob[CompilationUnit](ep(ep.finalType, new Neg, List(new Collect, new Eval, new PrettyP)))
//
//  lazy val results = EmptyResults().addAll(jobs.run())

}
