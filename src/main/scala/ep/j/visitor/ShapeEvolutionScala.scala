package ep.j.visitor   /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import ep.cls.CodeGenerationController
import ep.domain.{ShapeDomain, WithDomain}
import ep.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class ShapeFoundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {
  val gen:WithDomain[ShapeDomain] with VisitorGenerator with JUnitTestGenerator

  //lazy val processed:gen.domain.Model = gen.getProcessedModel   // process model as necessary
  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.generatedCode() ++
    gen.generateSuite(Some("expression"))

  override val routingPrefix: Option[String] = Some("scalaVisitor")
  override lazy val controllerAddress:String = gen.getModel.name
}

class S0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends ShapeFoundation(web, app) {
  override val gen = new WithDomain(ShapeDomain) with VisitorGenerator with JUnitTestGenerator with s0
}

class S1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends ShapeFoundation(web, app) {
  override val gen = new WithDomain(ShapeDomain) with VisitorGenerator with JUnitTestGenerator with s0 with s1
}