package ep.j.extensibleVisitor      /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import ep.j._
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {
  val gen:WithDomain[MathDomain] with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator

  // since this "extends" the visitor pattern, we must stay within that package "visitor"
  override lazy val generatedCode:Seq[CompilationUnit] =
    gen.generatedCode() ++
    gen.generateSuite(Some("visitor"))

  override val routingPrefix: Option[String] = Some("extensibleVisitor")
  override lazy val controllerAddress:String = gen.getModel.name
}
//
//class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0
//}
//
//class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0 with e1
//}
//
//class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0 with e1 with e2
//}
//
//class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0 with e1 with e2 with e3
//}
//
//class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0 with e1 with e2 with e3 with e4
//}
//
//class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
//}
//
//class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
//}
//
//class C1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
//}
