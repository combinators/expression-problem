package org.combinators.ep.language.gj.deployment.wadler
/* Generated: Thu Jun 06 22:28:16 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.gj._
import org.combinators.ep.language.gj.wadler._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * wadler solution in gj for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with WadlerGenerator with UnitTestGenerator with e0
}
/* 
 * wadler solution in gj for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with WadlerGenerator with UnitTestGenerator with e0 with e1
}
