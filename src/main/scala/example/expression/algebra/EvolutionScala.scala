package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.Domain
import example.expression.j.{TestGenerator, e0, e1, e2, e3, e4}
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:AlgebraGenerator with TestGenerator
  val model:gen.domain.Model

  override lazy val generatedCode:Seq[CompilationUnit] =
    model.types.map(tpe =>  gen.generateExp(model, tpe)) ++      // one class for each sub-type
    model.ops.map(op => gen.operationGenerator(model, op)) :+    // one class for each op
      gen.generateBaseClass() :+                                 // abstract base class
      gen.generateBase(model) :+                                 // visitor gets its own class (overriding concept)
      gen.generateSuite(Some("expression"))                      // generate test cases as well

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("scalaVisitor")
  override lazy val controllerAddress:String = model.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class xE0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with TestGenerator with e0 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e0
}

class xE1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with TestGenerator with e0 with e1 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e1
}

class xE2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with TestGenerator with e0 with e1 with e2 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e2
}

class xE3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with TestGenerator with e0 with e1 with e2 with e3 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e3
}

class xE4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with TestGenerator with e0 with e1 with e2 with e3 with e4 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e4
}