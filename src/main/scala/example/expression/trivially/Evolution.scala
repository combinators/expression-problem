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

  // flatten hierarchy and remove producer operations (i.e., those that are not compatible with this approach)
  lazy val reduced:gen.domain.Model = gen.compatible(model)
  lazy val flat:gen.domain.Model = gen.compatible(reduced.flat())
  override lazy val generatedCode:Seq[CompilationUnit] =
    flat.types.map(tpe => gen.generateExp(reduced, tpe)) ++     // one class for each sub-type
      gen.generateInterfaces(reduced) :+                        // interfaces for all subtypes
      gen.generateBase(reduced) :+                              // base  interface
      gen.generateSuite(Some("trivially"))                      // generate test cases as well

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("trivially")
  override lazy val controllerAddress:String = model.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.e0
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.e1
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 with e2 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.e2
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 with e2 with e3 {
    override val domain = new MathDomain{ }
  }
  override val model = gen.domain.e3
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGeneratorWithModel with e0 with e1 with e2 with e3 with e4 {
    override val domain = new MathDomain{ }

    // Interpreter Test Generator needs access to the model
    def getModel:domain.Model = {
      domain.e4
    }
  }
  override val model = gen.getModel
}

class E5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new TriviallyGenerator with TestGeneratorWithModel with e0 with e1 with e2 with e3 with e4 with e5 {
    override val domain = new MathDomain{ }

    // Interpreter Test Generator needs access to the model
    def getModel:domain.Model = {
      domain.e5
    }
  }
  override val model = gen.getModel
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  // Merge e3 with i2: without adding extra code to synthesize concepts, this will fail
  override val gen = new TriviallyGenerator with TestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with m1 {
    override val domain = new MergedMathDomain { }
  }

  override val model = gen.domain.m1
}
