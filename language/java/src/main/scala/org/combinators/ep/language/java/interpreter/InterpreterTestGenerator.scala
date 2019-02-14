package org.combinators.ep.language.java.interpreter

/*DI:LD:AD*/

import com.github.javaparser.ast.expr.SimpleName
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.domain.math.M0
import org.combinators.ep.generator.LanguageIndependentTestGenerator
import org.combinators.ep.language.java.{JUnitTestGenerator, JavaGenerator}
import org.combinators.templating.twirl.Java

/**
  * Interpreter needs to know the last model with operations for the given vertex in the extension graph.
  *
  * Note that we need LitInst for our test generation, so we just grab from M0
  */
trait InterpreterTestGenerator extends JUnitTestGenerator with JavaGenerator with LanguageIndependentTestGenerator {
  self: InterpreterGenerator =>
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Interpreter needs a function to get the active model. */
  def getModel:domain.Model

  abstract override def testTypeConverter(typeRep: TypeRep) : Type = {
    if (typeRep == baseTypeRep) { Java(modelInterfaceName(getModel)).tpe() }
    else super.testTypeConverter(typeRep)
  }

  override def inst(exp:domain.DataType, params:Expression*): CodeBlockWithResultingExpressions = {
    val name = exp.name
    val model = getModel
    val classify:SimpleName = Java(model.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")).simpleName()

    CodeBlockWithResultingExpressions(
      Java(s"new $classify$name${params.mkString("(", ", ", ")")}").expression[InstanceExpression]()
    )
  }
}
