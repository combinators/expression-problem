package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.Domain
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:AlgebraGenerator with AlgebraTestGenerator
  val model:gen.domain.Model                                  // maintain backwards history from current position

  lazy val reduced:gen.domain.Model = gen.compatible(model)
  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.processModel(reduced.inOrder) :+
    gen.generateSuite(Some("algebra"), reduced) :+                     // generate test cases as well
    gen.combinedAlgebra(Some("algebra"), reduced)

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("algebra")
  override lazy val controllerAddress:String = reduced.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class E0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with AlgebraTestGenerator with e0 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e0
}

class E1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with AlgebraTestGenerator with e0 with e1 {
    override val domain = new Domain{ }
  }

  override val model = gen.domain.e1
}

class E2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e2
}

class E3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e3
}

class E4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4 {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e4
}

// Still not ready to have Equals.

class E5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {

  override val gen = new AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with ex {
    override val domain = new Domain{ }
  }
  override val model = gen.domain.e5
}
