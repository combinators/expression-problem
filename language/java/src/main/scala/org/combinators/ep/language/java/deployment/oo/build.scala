package org.combinators.ep.language.java.deployment.oo
/* Generated: Wed Jun 26 17:53:46 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.java._
import org.combinators.ep.language.java.oo._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * oo solution in java for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
  override val gen = OOGenerator(e0(), JavaBinaryMethod(M0))
}
/* 
 * oo solution in java for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
  lazy val test = JUnitTestGenerator()
  override val gen = OOGenerator(e1(e0()), JavaNameProvider, JavaBinaryMethod(M0), JavaGenerator(JavaNameProvider), test)
}
/* 
 * oo solution in java for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2
}
/* 
 * oo solution in java for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
}
/* 
 * oo solution in java for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
}
/* 
 * oo solution in java for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}
/* 
 * oo solution in java for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
/* 
 * oo solution in java for M7
 * 
 * @group evolutions 
 */
class M7_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
}
/* 
 * oo solution in java for M8
 * 
 * @group evolutions 
 */
class M8_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 with e8
}
/* 
 * oo solution in java for I1
 * 
 * @group evolutions 
 */
class I1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with i1
}
/* 
 * oo solution in java for I2
 * 
 * @group evolutions 
 */
class I2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2
}
/* 
 * oo solution in java for P1
 * 
 * @group evolutions 
 */
class P1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with p1
}
/* 
 * oo solution in java for S0
 * 
 * @group evolutions 
 */
class S0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends ShapeFoundation(web, app) {
override val gen = new WithDomain(shape.ShapeDomain) with OOGenerator with JUnitTestGenerator with s0
}
/* 
 * oo solution in java for S1
 * 
 * @group evolutions 
 */
class S1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends ShapeFoundation(web, app) {
override val gen = new WithDomain(shape.ShapeDomain) with OOGenerator with JUnitTestGenerator with s0 with s1
}
/* 
 * oo solution in java for C1
 * 
 * @group evolutions 
 */
class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(math.MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2 with e2 with e3 with c1
}
