package org.combinators.ep.language.haskell.deployment.grow
/* Generated: Thu Jun 06 22:28:16 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.haskell._
import org.combinators.ep.language.haskell.grow._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * grow solution in haskell for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0
}
/* 
 * grow solution in haskell for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1
}
/* 
 * grow solution in haskell for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2
}
/* 
 * grow solution in haskell for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3
}
/* 
 * grow solution in haskell for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4
}
/* 
 * grow solution in haskell for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}
/* 
 * grow solution in haskell for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
