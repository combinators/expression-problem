package example.expression.interpreter /*DD:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.{MathDomain, WithDomain}
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
  extends CodeGenerationController[CompilationUnit](web, app) {
  val gen:WithDomain[MathDomain] with InterpreterGenerator with InterpreterTestGenerator

  override lazy val generatedCode:Seq[CompilationUnit] =
      gen.generatedCode() ++
      gen.generateSuite(Some("interpreter"))

  override val routingPrefix: Option[String] = Some("interpreter")
  override lazy val controllerAddress:String = gen.getModel.name
}

class M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0
}

class M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1
}

class M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2
}

class M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3
}

class M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4
}

class M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
}

class M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends Foundation(web, app) {
  override val gen = new WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
}
