package example.expression.trivially

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.{MathDomain, WithDomain, companionMathDomain}
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:WithDomain[MathDomain] with TriviallyGenerator with TestGenerator

  lazy val processed:gen.domain.Model = gen.getProcessedModel   // process model as necessary
  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.generatedCode(processed) :+
    gen.generateSuite(Some("trivially"))

  override val routingPrefix: Option[String] = Some("trivially")
  override lazy val controllerAddress:String = gen.getModel.name
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with TriviallyGenerator with TestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with TriviallyGenerator with TestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with TriviallyGenerator with TestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with TriviallyGenerator with TestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with TriviallyGenerator with TestGeneratorWithModel with e0 with e1 with e2 with e3 with e4
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with TriviallyGenerator with TestGeneratorWithModel with e0 with e1 with e2 with e3 with e4 with ex with e5
}

class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with TriviallyGenerator with TestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
}
