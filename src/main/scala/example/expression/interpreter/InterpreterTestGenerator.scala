package example.expression.interpreter

import com.github.javaparser.ast.expr.{Expression, SimpleName}
import example.expression.domain.M0
import example.expression.j.TestGenerator
import org.combinators.templating.twirl.Java

/**
  * Interpreter needs to know the last model with operations for the given vertex in the extension graph.
  *
  * Note that we need LitInst for our test generation, so we just grab from M0
  */
trait InterpreterTestGenerator extends TestGenerator with M0 {

  /** Interpreter needs a function to get the active model. */
  def getModel:domain.Model

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(inst:domain.AtomicInst) : Expression = {
    val name = inst.e.name

    val model = getModel
    val classify:SimpleName = Java(model.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")).simpleName()

    inst match {
      case lit:LitInst => Java(s"new $classify$name(${lit.i.get.toString})").expression()
      case ui:domain.UnaryInst =>
        Java(s"new $classify$name(${convert(ui.inner)})").expression()
      case bi:domain.BinaryInst =>
        Java(s"new $classify$name(${convert(bi.left)}, ${convert(bi.right)})").expression()

      case _ =>  Java(s""" "unknown $name" """).expression()
    }
  }
}






