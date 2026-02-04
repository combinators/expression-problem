package org.combinators.ep.language.gj.wadler    /*DD:LD:AD*/

import org.combinators.ep.language.gj.GJWithPathPersistable._
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.language.gj._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[GJWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with WadlerGenerator with UnitTestGenerator

override lazy val generatedCode:Seq[GJWithPath] =
    gen.generatedCode() ++
    gen.generateSuite(routingPrefix)

  override val routingPrefix: Option[String] = Some("wadler")
  override lazy val controllerAddress:String = gen.getModel.name
}
