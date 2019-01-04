package example.expression.interpreter   /*DI:LD:AD*/

import com.github.javaparser.ast.expr.SimpleName
import example.expression.domain.M0
import example.expression.j.JUnitTestGenerator
import org.combinators.templating.twirl.Java

/**
  * Interpreter needs to know the last model with operations for the given vertex in the extension graph.
  *
  * Note that we need LitInst for our test generation, so we just grab from M0
  */
trait InterpreterTestGenerator extends JUnitTestGenerator with M0 {

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

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  override def convertRecursive(inst: domain.Binary, left:String, right:String): Expression = {
    val model = getModel
    val name = inst.name
    val classify:SimpleName = Java(model.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")).simpleName()

    Java(s"new $classify$name($left, $right)").expression()
  }

  /** Type to use when referring to specific instance. */
  override def exprDefine(exp:domain.AtomicInst) : Type = {
    val name = exp.e.name

    val model = getModel
    val classify:SimpleName = Java(model.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")).simpleName()

    Java(s"$classify$name").tpe()
  }

}
