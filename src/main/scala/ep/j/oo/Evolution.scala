package ep.j.oo  /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import ep.cls.CodeGenerationController
import ep.domain.{MathDomain, WithDomain}
import ep.j._
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.templating.persistable.JavaPersistable._

/**
  * Straight OO solution to EP.
  *
  * Generates CompilationUnit artifacts representing the Java implementation.
  *
  * @groupname evolutions  Evolutions
  * @groupprio evolutions 0
  */
abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {

  /** Generator uses Straight OO solution for EP with JUnit test cases. */
  val gen:WithDomain[MathDomain] with OOGenerator with JUnitTestGenerator

  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.generatedCode() ++
      gen.generateSuite(routingPrefix)

  override val routingPrefix: Option[String] = Some("oo")
  override lazy val controllerAddress:String = gen.getModel.name
}

/**
  * OO solution in Java for M0 domain.
  *
  * @group evolutions
  */
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0
}

/**
  * OO solution in Java for M1 domain.
  *
  * @group evolutions
  */
class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1
}

/**
  * OO solution in Java for M2 domain.
  *
  * @group evolutions
  */
class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2
}

/**
  * OO solution in Java for M3 domain.
  * @group evolutions
  */
class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
}

/**
  * OO solution in Java for M4 domain.
  *
  * @group evolutions
  */
class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
}

/**
  * OO solution in Java for M5 domain.
  *
  * @group evolutions
  */
class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}

/**
  * OO solution in Java for M6 domain.
  *
  * @group evolutions
  */
class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}

/**
  * OO solution in Java for I2 domain, an independent branch M0 -> M1 -> I1 -> I2.
  *
  * @group evolutions
  */
class I2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2
}

/**
  * OO solution that joins together (M0 -> M1 -> M2 -> M3) with (M0 -> M1 -> I1 -> I2).
  */
class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
}
