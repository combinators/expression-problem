package org.combinators.ep.language.java.visitor   /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
//import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.shape.ShapeDomain
import org.combinators.ep.language.java.{JUnitTestGenerator, s0, s1}
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class ShapeFoundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {
//  val gen:WithDomain[ShapeDomain] with VisitorGenerator with JUnitTestGenerator
//
//  override lazy val generatedCode:Seq[CompilationUnit] =
//    gen.generatedCode() ++
//    gen.generateSuite(routingPrefix)
//
//  override val routingPrefix: Option[String] = Some("visitor")
//  override lazy val controllerAddress:String = gen.getModel.name
}
