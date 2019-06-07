package org.combinators.ep.language.cpp.deployment.oo
/* Generated: Thu Jun 06 22:28:16 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.cpp._
import org.combinators.ep.language.cpp.oo._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * oo solution in cpp for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0
}
/* 
 * oo solution in cpp for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1
}
/* 
 * oo solution in cpp for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2
}
/* 
 * oo solution in cpp for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3
}
/* 
 * oo solution in cpp for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4
}
/* 
 * oo solution in cpp for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5
}
/* 
 * oo solution in cpp for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5 with cpp_e6
}
