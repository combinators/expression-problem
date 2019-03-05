package org.combinators.ep.language.java.oo   /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.language.java.JUnitTestGenerator
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
