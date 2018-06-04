package example.expression.Straight

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.Domain
import example.expression.j.TestGenerator
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController
import org.combinators.templating.persistable.JavaPersistable._

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:StraightGenerator with TestGenerator
  val model:gen.domain.Model

  override lazy val generatedCode:Seq[CompilationUnit] =
    model.types.map(tpe =>  gen.generateExp(model, tpe)) :+      // one class for each sub-type
      gen.generateBase(model) :+                                 // base class $BASE
      gen.generateSuite(None)                                    // generate test cases as well

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("straight")
  override lazy val controllerAddress:String = model.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new StraightGenerator with E0_Generator {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e0
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new StraightGenerator with E0_Generator with E1_Generator {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e1
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new StraightGenerator with E0_Generator with E1_Generator with E2_Generator {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e2
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new StraightGenerator with E0_Generator with E1_Generator with E2_Generator with E3_Generator {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e3
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new StraightGenerator with E0_Generator with E1_Generator with E2_Generator with E3_Generator with E4_Generator {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e4
}