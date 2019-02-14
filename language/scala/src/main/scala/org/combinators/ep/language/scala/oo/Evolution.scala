package org.combinators.ep.language.scala.oo  /*DD:LD:AD*/

import org.combinators.ep.language.scala.ScalaWithPathPersistable._
import javax.inject.Inject
import org.combinators.cls.git.Results
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.generator.FileWithPath
import org.combinators.ep.language.scala.ScalaWithPath
import org.combinators.ep.generator.FileWithPathPersistable._

abstract class FoundationOO @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[ScalaWithPath](web, app)
  {
    val gen:WithDomain[MathDomain] with OderskyGenerator with FunSpecOOTestGenerator

    override lazy val generatedCode:Seq[ScalaWithPath] =
      gen.generatedCode() ++
      gen.generateSuite(routingPrefix, Some(gen.getModel))

    /**
      * Add all helper classes to be external artifacts.
      * Has to be lazy so subclasses can compute model.
      */
    override lazy val results:Results = gen.getsbt().foldLeft(defaultResults(generatedCode))((former, next) => former.addExternalArtifact[FileWithPath](next))

    override val routingPrefix: Option[String] = Some("odersky")
    override lazy val controllerAddress:String = gen.getModel.name
  }

