package example.expression.scalaVisitor

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.{ShapeDomain, WithDomain, companionShapeDomain}
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class ShapeFoundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:WithDomain[ShapeDomain] with VisitorGenerator with TestGenerator

  lazy val processed:gen.domain.Model = gen.getProcessedModel   // process model as necessary
  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.generatedCode(processed) :+
    gen.generateSuite(Some("expression"))

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("scalaVisitor")
  override lazy val controllerAddress:String = gen.getModel.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class S0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends ShapeFoundation(web, app) {
  override val gen = new WithDomain(companionShapeDomain) with VisitorGenerator with TestGenerator with s0
}

class S1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends ShapeFoundation(web, app) {
  override val gen = new WithDomain(companionShapeDomain) with VisitorGenerator with TestGenerator with s0 with s1
}

//not yet
//class S2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends ShapeFoundation(web, app) {
//
//  override val gen = new VisitorGenerator with TestGenerator with s0 with s1 with s2 {
//    override val domain = new ShapeDomain{ }
//  }
//  override val model = gen.domain.s2
//}