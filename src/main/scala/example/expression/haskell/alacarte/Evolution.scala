package example.expression.haskell.alacarte

/*DD:LD:AD*/

import example.expression.domain.{MathDomain, WithDomain}
import example.expression.haskell.HaskellWithPathPersistable._
import example.expression.haskell._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class FoundationHaskell @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[HaskellWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with ALaCarteGenerator with ALaCarteTestGenerator

override lazy val generatedCode:Seq[HaskellWithPath] =
    gen.generatedCode() ++
    gen.generateSuite()

  override val routingPrefix: Option[String] = Some("haskell")
  override lazy val controllerAddress:String = gen.getModel.name
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteProducer with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteProducer with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}

class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteProducer with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}