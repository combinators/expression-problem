package org.combinators.ep.language.haskell.deployment.alacarte
/* Generated: Wed Jun 26 17:53:46 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.haskell._
import org.combinators.ep.language.haskell.alacarte._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * alacarte solution in haskell for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0
}
/* 
 * alacarte solution in haskell for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1
}
/* 
 * alacarte solution in haskell for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2
}
/* 
 * alacarte solution in haskell for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3
}
/* 
 * alacarte solution in haskell for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4
}
/* 
 * alacarte solution in haskell for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}
/* 
 * alacarte solution in haskell for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
