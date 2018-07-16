package example.expression.algebra  /*DD:LD:AD*/

/**
  * Extensibility for the Masses
  * Bruno C. d. S. Oliveira & William R. Cook
  * ECOOP 2012
  * https://dl.acm.org/citation.cfm?id=2367167
  */

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.{MathDomain, WithDomain}
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {
  val gen:WithDomain[MathDomain] with AlgebraGenerator with AlgebraTestGenerator

  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.generatedCode() :+
    gen.generateSuite(Some("algebra"), Some(gen.process(gen.getModel))) :+
    gen.combinedAlgebra(Some("algebra"), gen.process(gen.getModel))       // requires a combined algebra for testing

  override val routingPrefix: Option[String] = Some("algebra")
  override lazy val controllerAddress:String = gen.getModel.name
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4
}

// Still not ready to have Equals.
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with ex
}

class I2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with i1 with i2
}

class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
}
