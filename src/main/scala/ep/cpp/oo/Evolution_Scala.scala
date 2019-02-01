package ep.cpp.oo     /*DD:LD:AD*/

import ep.cls.CodeGenerationController
import ep.cpp.CPPFileUtils._
import ep.cpp._
import ep.domain.{MathDomain, WithDomain}
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class FoundationCPP @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CPPFile](web, app)
{
  val gen:WithDomain[MathDomain] with StraightGenerator with TestGenerator

override lazy val generatedCode:Seq[CPPFile] =
    gen.generatedCode() ++
    gen.generateBinaryMethodHelpers() ++
    gen.generateSuite(routingPrefix)

  override val routingPrefix: Option[String] = Some("cpp_oo")
  override lazy val controllerAddress:String = gen.getModel.name
}

class CPP_M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0
}

class CPP_M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1
}

class CPP_M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2
}

class CPP_M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3
}

class CPP_M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4
}

class CPP_M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5
}

class CPP_M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationCPP(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5 with cpp_e6
}