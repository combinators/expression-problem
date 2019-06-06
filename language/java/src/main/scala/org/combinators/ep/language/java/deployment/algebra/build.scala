package org.combinators.ep.language.java.deployment.algebra

/* Generated: Sun Feb 10 22:29:19 EST 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.language.java._
import org.combinators.ep.language.java.algebra._
import javax.inject.Inject
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.language.java.algebra.{AlgebraGenerator, AlgebraTestGenerator, Foundation}
import org.combinators.ep.language.java._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * algebra solution in j for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0
}
/* 
 * algebra solution in j for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1
}
/* 
 * algebra solution in j for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2
}
/* 
 * algebra solution in j for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3
}
/* 
 * algebra solution in j for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4
}
/* 
 * algebra solution in j for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}
/* 
 * algebra solution in j for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
/* 
 * algebra solution in j for I1
 * 
 * @group evolutions 
 */
class I1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with i1
}
/* 
 * algebra solution in j for I2
 * 
 * @group evolutions 
 */
class I2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with i1 with i2
}
/* 
 * algebra solution in j for C1
 * 
 * @group evolutions 
 */
class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with i1 with i2 with e2 with e3 with c1
}
/*
 * algebra solution in j for P1
 *
 * @group evolutions
 */
class P1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with p1
}