package org.combinators.ep.language.cpp.deployment.visitor
/* Generated: Wed Jun 26 17:53:47 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.cpp._
import org.combinators.ep.language.cpp.visitor._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * visitor solution in cpp for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0
}
/* 
 * visitor solution in cpp for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1
}
/* 
 * visitor solution in cpp for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2
}
/* 
 * visitor solution in cpp for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3
}
/* 
 * visitor solution in cpp for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4
}
/* 
 * visitor solution in cpp for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5
}
/* 
 * visitor solution in cpp for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5 with cpp_e6
}
