package example.expression.haskell

/*DD:LD:AD*/

import example.expression.domain.{MathDomain, WithDomain}
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

import example.expression.haskell.HaskellWithPathPersistable._

abstract class FoundationHaskell @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[HaskellWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with ALaCarteGenerator with TestGenerator

override lazy val generatedCode:Seq[HaskellWithPath] =
    gen.generatedCode() ++
    gen.generateSuite()

  override val routingPrefix: Option[String] = Some("haskell")
  override lazy val controllerAddress:String = gen.getModel.name
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with TestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with TestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with TestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskell(web, app) {
  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with TestGenerator with e0 with e1 with e2 with e3
}