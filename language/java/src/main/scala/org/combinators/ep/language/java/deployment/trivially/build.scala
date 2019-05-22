package org.combinators.ep.language.java.deployment.trivially

/* Generated: Sun Feb 10 22:29:19 EST 2019 */
import org.combinators.ep.domain.math._
import javax.inject.Inject
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.language.java._
import org.combinators.ep.language.java.trivially.{Foundation, TriviallyGenerator, TriviallyTestGenerator}
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * trivially solution in j for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0
}
/* 
 * trivially solution in j for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1
}
/* 
 * trivially solution in j for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with e2
}
/* 
 * trivially solution in j for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with e2 with e3
}
/* 
 * trivially solution in j for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with e2 with e3 with e4
}
/* 
 * trivially solution in j for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}
/* 
 * trivially solution in j for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
/* 
 * trivially solution in j for I1
 * 
 * @group evolutions 
 */
class I1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with i1
}
/* 
 * trivially solution in j for I2
 * 
 * @group evolutions 
 */
class I2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with i1 with i2
}
/* 
 * trivially solution in j for C1
 * 
 * @group evolutions 
 */
class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with TriviallyGenerator with TriviallyTestGenerator with e0 with e1 with i1 with i2 with e2 with e3 with c1
}