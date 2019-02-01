package ep.scala.functional  /*DD:LD:AD*/

import ep.cls.CodeGenerationController
import ep.domain.{MathDomain, WithDomain}
import ep.scala.ScalaWithPathPersistable._
import ep.scala._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class FoundationGrows @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[ScalaWithPath](web, app)
  {
    val gen:WithDomain[MathDomain] with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator

    override lazy val generatedCode:Seq[ScalaWithPath] =
      gen.generatedCode() ++
      gen.generateSuite(routingPrefix, Some(gen.getModel))

    override val routingPrefix: Option[String] = Some("scala_func")
    override lazy val controllerAddress:String = gen.getModel.name
  }

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator with e0 with e1 with e2 with e3 with e4
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}

class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with FunctionalTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}