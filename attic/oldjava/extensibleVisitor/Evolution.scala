package org.combinators.ep.language.java.extensibleVisitor   /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
//import org.combinators.ep.domain.WithDomain
//import org.combinators.ep.domain.math.MathDomain
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) {
// GOING AWAY
//  extends CodeGenerationController[CompilationUnit](web, app) {
//  val gen:WithDomain[MathDomain] with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator
//
//  // since this "extends" the visitor pattern, we must stay within that package "visitor"
//  override lazy val generatedCode:Seq[CompilationUnit] =
//    gen.generatedCode() ++
//    gen.generateSuite(Some("visitor"))
//
//  override val routingPrefix: Option[String] = Some("extensibleVisitor")
//  override lazy val controllerAddress:String = gen.getModel.name
}
