package example.expression.interpreter

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.Domain
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

// Bruce2003	Solving Expression problem using Interpreter Pattern
// Some challenging typing issues in object-oriented languages
// TCS (82) 2003
// http://www.cs.pomona.edu/~kim/ftp/WOOD.pdf


abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:InterpreterGenerator with InterpreterTestGenerator
  val model:gen.domain.Model

  // flatten hierarchy and remove producer operations (i.e., those that are not compatible with this approach)
  lazy val reduced:gen.domain.Model = gen.compatible(model)
  //lazy val flat:gen.domain.Model = gen.compatible(reduced.flat())
  override lazy val generatedCode:Seq[CompilationUnit] =
      // one interface for every model that contains an operation
      reduced.inOrder.filter(model => model.ops.nonEmpty).map(model => gen.generateBase(model)) ++      // Each operation gets interface
   //   reduced.inOrder.flatMap(model => model.types.map(exp => gen.generateExp(model, exp)))     ++
      reduced.inOrder.filter(model => model.ops.nonEmpty).flatMap(model => gen.generateBaseExtensions(model)) ++   // Each operation must provide class implementations for all past dataTypes
      gen.generateIntermediateTypes(model) :+                    // New types added in between operations need to be manifest
      gen.generateSuite(Some("interpreter"), reduced)                          // generate test cases as well

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("interpreter")
  override lazy val controllerAddress:String = model.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new InterpreterGenerator with InterpreterTestGenerator with e0 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e0
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new InterpreterGenerator with InterpreterTestGenerator with e0 with e1 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e1
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e2
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e3
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e4
}

class E5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e5
}