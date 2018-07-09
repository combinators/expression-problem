package example.expression.interpreter

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.{MathDomain, WithDomain, companionMathDomain}
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

// Bruce2003	Solving Expression problem using Interpreter Pattern
// Some challenging typing issues in object-oriented languages
// TCS (82) 2003
// http://www.cs.pomona.edu/~kim/ftp/WOOD.pdf


abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:WithDomain[MathDomain] with InterpreterGenerator with InterpreterTestGenerator

  lazy val processed:gen.domain.Model = gen.getProcessedModel   // process model as necessary
  override lazy val generatedCode:Seq[CompilationUnit] =
      gen.generatedCode(processed) :+
      gen.generateSuite(Some("interpreter"))

  override val routingPrefix: Option[String] = Some("interpreter")
  override lazy val controllerAddress:String = gen.getModel.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(companionMathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with ex with e5
}