package example.expression.oo

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.ShapeDomain
import example.expression.j._
import javax.inject.Inject
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import shared.compilation.CodeGenerationController

// https://www.cs.rice.edu/~cork/teachjava/2003/readings/visitor1.pdf

abstract class ShapeFoundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CompilationUnit](web, app)
{
  val gen:OOGenerator with TestGenerator
  val model:gen.domain.Model

  lazy val flat:gen.domain.Model = model.flat()
  override lazy val generatedCode:Seq[CompilationUnit] =
    flat.types.map (tpe => gen.generateExp(flat, tpe)) :+     // one class for each sub-type
      gen.generateBase(flat) :+                               // base class $BASE
      gen.generateSuite(Some("oo"))                           // generate test cases as well

  // request by "git clone -b variation_0 http://localhost:9000/straight/eN/eN.git" where N is a version #
  override val routingPrefix: Option[String] = Some("oo")
  override lazy val controllerAddress:String = model.name
}

// also: don't forget that entries need to be in place in routes file. These specifications can
// be viewed as the 'architecture' of the EP solution.
class S0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends ShapeFoundation(web, app) {

  override val gen = new OOGenerator with TestGenerator with s0 {
    override val domain = new ShapeDomain{ }
  }
  override val model = gen.domain.s0
}

class S1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends ShapeFoundation(web, app) {

  override val gen = new OOGenerator with TestGenerator with s0 with s1 {
    override val domain = new ShapeDomain{ }
  }
  override val model = gen.domain.s1
}
