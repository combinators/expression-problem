package ep.gj.wadler    /*DD:LD:AD*/

import ep.gj.GJWithPathPersistable._
import ep.gj._
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[GJWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with WadlerGenerator with TestGenerator

override lazy val generatedCode:Seq[GJWithPath] =
    gen.generatedCode() ++
    gen.generateSuite(routingPrefix)

  override val routingPrefix: Option[String] = Some("gj")
  override lazy val controllerAddress:String = gen.getModel.name
}

//class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with WadlerGenerator with TestGenerator with e0
//}
//
//class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with WadlerGenerator with TestGenerator with e0 with e1
//}
