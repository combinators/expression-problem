package org.combinators.ep.language.java.deployment.interpreter

/* Generated: Sun Feb 10 22:29:19 EST 2019 */
import javax.inject.Inject
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math._
import org.combinators.ep.language.java._
import org.combinators.ep.language.java.interpreter.{Foundation, InterpreterGenerator, InterpreterTestGenerator}
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle


class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}

class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
