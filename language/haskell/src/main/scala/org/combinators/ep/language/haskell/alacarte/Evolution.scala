package org.combinators.ep.language.haskell.alacarte   /*DD:LD:AD*/

import org.combinators.ep.language.haskell.HaskellWithPathPersistable._
import javax.inject.Inject
import org.combinators.cls.git.Results
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.language.haskell.HaskellWithPath
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[HaskellWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with ALaCarteGenerator with ALaCarteTestGenerator

  lazy val generatedCode:Seq[HaskellWithPath] =
    gen.generatedCode() ++
    gen.generateSuite()

  /**
    * Add all helper classes to be external artifacts.
    * Has to be lazy so subclasses can compute model.
    */
  override lazy val results:Results = gen.helperClasses().foldLeft(defaultResults(generatedCode))((former, next) => former.addExternalArtifact[HaskellWithPath](next))

  override val routingPrefix: Option[String] = Some("alacarte")
  override lazy val controllerAddress:String = gen.getModel.name
}

//class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0
//}
//
//class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1
//}
//
//class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2
//}
//
//class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3
//}
//
//class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteProducer with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4
//}
//
//class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteProducer with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
//}
//
//class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteProducer with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
//}
