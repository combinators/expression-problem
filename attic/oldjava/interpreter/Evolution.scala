package org.combinators.ep.language.java.interpreter  /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import javax.inject.Inject
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

// Bruce2003	Solving Expression problem using Interpreter Pattern
// Some challenging typing issues in object-oriented languages
// TCS (82) 2003
// http://www.cs.pomona.edu/~kim/ftp/WOOD.pdf

abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app) {
  val gen:WithDomain[MathDomain] with InterpreterGenerator with InterpreterTestGenerator

  override lazy val generatedCode:Seq[CompilationUnit] =
      gen.generatedCode() ++
      gen.generateSuite(routingPrefix)

  override val routingPrefix: Option[String] = Some("interpreter")
  override lazy val controllerAddress:String = gen.getModel.name
}
