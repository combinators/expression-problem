package org.combinators.ep.language.java.algebra   /*DD:LD:AD*/

/**
  * Extensibility for the Masses
  * Bruno C. d. S. Oliveira & William R. Cook
  * ECOOP 2012
  * https://dl.acm.org/citation.cfm?id=2367167
  */

import com.github.javaparser.ast.CompilationUnit
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
//import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {
//  val gen:WithDomain[MathDomain] with AlgebraGenerator with AlgebraTestGenerator
//
//  //lazy val process = gen.process(gen.getModel)
//  override lazy val generatedCode:Seq[CompilationUnit] =
//    gen.generatedCode() ++
//    gen.generateSuite(Some("algebra")) :+
//    gen.combinedAlgebra(Some("algebra"))
//
//  override val routingPrefix: Option[String] = Some("algebra")
//  override lazy val controllerAddress:String = gen.getModel.name
}

