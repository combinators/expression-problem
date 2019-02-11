package build.gj.wadler
/* Generated: Sun Feb 10 22:29:20 EST 2019 */
import ep.domain._
import ep.gj._
import ep.gj.wadler._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * wadler solution in gj for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with WadlerGenerator with TestGenerator with e0
}
/* 
 * wadler solution in gj for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with WadlerGenerator with TestGenerator with e0 with e1
}
