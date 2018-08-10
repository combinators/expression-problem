package example.expression.cpp    /*DD:LD:AD*/

import example.expression.domain.{MathDomain, WithDomain}
import CPPFileUtils._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class FoundationCPP @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CPPFile](web, app)
{
  val gen:WithDomain[MathDomain] with CPPGenerator with TestGenerator

override lazy val generatedCode:Seq[CPPFile] =
    gen.generatedCode() ++
    gen.generateSuite(Some("cpp"))

  override val routingPrefix: Option[String] = Some("cpp")
  override lazy val controllerAddress:String = gen.getModel.name
}

class CPP_M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with CPPGenerator with TestGenerator with cpp_e0
}
