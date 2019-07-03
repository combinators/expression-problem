package org.combinators.ep.language.haskell.straight   /*DD:LD:AD*/

import org.combinators.ep.language.haskell.HaskellWithPathPersistable._
import javax.inject.Inject
import org.combinators.cls.git.Results
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.generator.FileWithPath
import org.combinators.ep.language.haskell.HaskellWithPath
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[HaskellWithPath](web, app)
{
  val gen:WithDomain[MathDomain] with StraightGenerator with StraightTestGenerator

  lazy val generatedCode:Seq[HaskellWithPath] =
    gen.generatedCode() ++
    gen.generateSuite(None)

  /**
    * Add all helper classes to be external artifacts.
    * Has to be lazy so subclasses can compute model.
    */
  override lazy val results:Results = gen.helperClasses().foldLeft(defaultResults(generatedCode))((former, next) => former.addExternalArtifact[HaskellWithPath](next))

  override val routingPrefix: Option[String] = Some("haskell_func")
  override lazy val controllerAddress:String = gen.getModel.name
}
