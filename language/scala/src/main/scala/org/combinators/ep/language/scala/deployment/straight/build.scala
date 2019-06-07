package org.combinators.ep.language.scala.deployment.straight
/* Generated: Thu Jun 06 22:28:16 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.scala._
import org.combinators.ep.language.scala.straight._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * straight solution in scala for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0
}
/* 
 * straight solution in scala for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1
}
/* 
 * straight solution in scala for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2
}
/* 
 * straight solution in scala for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3
}
/* 
 * straight solution in scala for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3 with e4
}
/* 
 * straight solution in scala for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}
/* 
 * straight solution in scala for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
