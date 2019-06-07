package org.combinators.ep.language.java.deployment.interpreter
/* Generated: Thu Jun 06 22:28:16 EDT 2019 */
import org.combinators.ep.domain.math._
import org.combinators.ep.domain._
import org.combinators.ep.language.java._
import org.combinators.ep.language.java.interpreter._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
/* 
 * interpreter solution in java for M0
 * 
 * @group evolutions 
 */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0
}
/* 
 * interpreter solution in java for M1
 * 
 * @group evolutions 
 */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1
}
/* 
 * interpreter solution in java for M2
 * 
 * @group evolutions 
 */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2
}
/* 
 * interpreter solution in java for M3
 * 
 * @group evolutions 
 */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3
}
/* 
 * interpreter solution in java for M4
 * 
 * @group evolutions 
 */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4
}
/* 
 * interpreter solution in java for M5
 * 
 * @group evolutions 
 */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}
/* 
 * interpreter solution in java for M6
 * 
 * @group evolutions 
 */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
/* 
 * interpreter solution in java for M7
 * 
 * @group evolutions 
 */
class M7_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
}
/* 
 * interpreter solution in java for I1
 * 
 * @group evolutions 
 */
class I1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with i1
}
/* 
 * interpreter solution in java for I2
 * 
 * @group evolutions 
 */
class I2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with i1 with i2
}
/* 
 * interpreter solution in java for P1
 * 
 * @group evolutions 
 */
class P1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with p1
}
/* 
 * interpreter solution in java for C1
 * 
 * @group evolutions 
 */
class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {
override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with i1 with i2 with e2 with e3 with c1
}
