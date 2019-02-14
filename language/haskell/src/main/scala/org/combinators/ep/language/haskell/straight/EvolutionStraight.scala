package org.combinators.ep.language.haskell.straight   /*DD:LD:AD*/

import org.combinators.ep.language.haskell.HaskellWithPathPersistable._
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.language.haskell.HaskellWithPath
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[HaskellWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with StraightGenerator with StraightTestGenerator

  override lazy val generatedCode:Seq[HaskellWithPath] =
    gen.generatedCode() ++
    gen.generateSuite()

  override val routingPrefix: Option[String] = Some("haskellStraight")
  override lazy val controllerAddress:String = gen.getModel.name
}
//
//class S0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0
//}
//
//class S1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1
//}
//
//class S2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2
//}
//
//class S3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3
//}
//
//class S4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4
//}
//
//class S5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
//}
//
//class S6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
//}
