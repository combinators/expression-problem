package example.expression.trivially

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.{MathDomain, MergedMathDomain}
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:TriviallyGenerator with TestGenerator
  val model:gen.domain.Model

  lazy val processed:gen.domain.Model = gen.apply(model)   // process model as necessary
  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.generatedCode(processed) :+
    gen.generateSuite(Some("trivially"))

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("trivially")
  override lazy val controllerAddress:String = model.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.m0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.m1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 with e2 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.m2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 with e2 with e3 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.m3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGeneratorWithModel with e0 with e1 with e2 with e3 with e4 {
    override val domain = new MathDomain{ }

    // Interpreter Test Generator needs access to the model
    def getModel:domain.Model = {
      domain.m4
    }
  }
  override val model = gen.getModel
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGeneratorWithModel with e0 with e1 with e2 with e3 with e4 with e5 {
    override val domain = new MathDomain{ }

    // Interpreter Test Generator needs access to the model
    def getModel:domain.Model = {
      domain.m5
    }
  }
  override val model = gen.getModel
}

class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  // Merge e3 with i2: without adding extra code to synthesize concepts, this will fail
  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1 {
    override val domain = new MergedMathDomain { }
  }

  override val model = gen.domain.c1
}
