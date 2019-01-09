package example.expression.haskell.straight   /*DD:LD:AD*/

import example.expression.domain.{MathDomain, WithDomain}
import example.expression.haskell.HaskellWithPathPersistable._
import example.expression.haskell._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class FoundationHaskellStraight @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[HaskellWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with StraightGenerator with StraightTestGenerator

  override lazy val generatedCode:Seq[HaskellWithPath] =
    gen.generatedCode() ++
    gen.generateSuite()

  override val routingPrefix: Option[String] = Some("haskellStraight")
  override lazy val controllerAddress:String = gen.getModel.name
}

class S0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskellStraight(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0
}

class S1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskellStraight(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1
}

class S2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskellStraight(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2
}

class S3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskellStraight(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3
}

class S4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskellStraight(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightProducer with StraightTestGenerator with e0 with e1 with e2 with e3 with e4
}

class S5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskellStraight(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightProducer with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}

class S6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationHaskellStraight(web, app) {
  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightProducer with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
