package example.expression.haskell.grow    /*DD:LD:AD*/

import example.expression.domain.{MathDomain, WithDomain}
import example.expression.haskell.HaskellWithPathPersistable._
import example.expression.haskell._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class FoundationGrows @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[HaskellWithPath](web, app)
  {
    val gen:WithDomain[MathDomain] with GrowGenerator with GrowTestGenerator

    override lazy val generatedCode:Seq[HaskellWithPath] =
      gen.generatedCode() ++
      gen.generateSuite(Some(gen.getModel))

    override val routingPrefix: Option[String] = Some("grow")
    override lazy val controllerAddress:String = gen.getModel.name
  }

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationGrows(web, app) {
  override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowProducer with GrowTestGenerator with e0 with e1 with e2 with e3 with e4
}
