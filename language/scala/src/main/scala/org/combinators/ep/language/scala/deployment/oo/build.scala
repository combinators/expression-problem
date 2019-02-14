package org.combinators.ep.language.scala.deployment.oo

import javax.inject.Inject
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.language.scala._
import org.combinators.ep.language.scala.oo._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationOO(web, app) {
  override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationOO(web, app) {
  override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationOO(web, app) {
  override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationOO(web, app) {
  override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationOO(web, app) {
  override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3 with e4
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationOO(web, app) {
  override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}

class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends FoundationOO(web, app) {
  override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
