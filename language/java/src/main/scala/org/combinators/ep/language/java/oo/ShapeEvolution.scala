package org.combinators.ep.language.java.oo    /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
//import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.shape.ShapeDomain
import org.combinators.ep.language.java.{JUnitTestGenerator, PerformanceTestGenerator, s0, s1}
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

/**
  * Synthesizing Object-Oriented and Functional Design to Promote Re-Use
  * Shriram Krishnamurthi, Matthias Felleisen, Daniel P. Friedman
  * European Conference on Object-Oriented Programming
  * https://cs.brown.edu/~sk/Publications/Papers/Published/kff-synth-fp-oo/
  */
abstract class ShapeFoundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {
//  val gen:WithDomain[ShapeDomain] with OOGenerator with JUnitTestGenerator
//
//  override lazy val generatedCode:Seq[CompilationUnit] =
//    gen.generatedCode() ++
//      gen.generateSuite(routingPrefix)                             // generate test cases as well
//
//  override val routingPrefix: Option[String] = Some("oo")
//  override lazy val controllerAddress:String = gen.getModel.name
}
