package example.expression.Pure

import com.github.javaparser.ast.CompilationUnit
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

import org.combinators.templating.persistable.JavaPersistable._

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:AbstractGenerator
  val model:gen.pure.Model

  override lazy val generatedCode:Seq[CompilationUnit] =
    model.types.map(tpe =>  gen.generateExp(model, tpe)) :+      // one class for each sub-type
      gen.generateBase(model) :+                                 // base class straight
      gen.generateSuite()                                        // generate test cases as well

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("straight")
  override lazy val controllerAddress:String = model.name
}

class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AbstractGenerator with E0_Generator {
    override val pure = new Pure{ }
  }
  override val model = gen.pure.e0
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AbstractGenerator with E0_Generator with E1_Generator {
    override val pure = new Pure{ }
  }
  override val model = gen.pure.e1
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AbstractGenerator with E0_Generator with E1_Generator with E2_Generator {
    override val pure = new Pure{ }
  }
  override val model = gen.pure.e2
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AbstractGenerator with E0_Generator with E1_Generator with E2_Generator with E3_Generator {
    override val pure = new Pure{ }
  }
  override val model = gen.pure.e3
}