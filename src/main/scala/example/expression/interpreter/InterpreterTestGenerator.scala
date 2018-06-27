package example.expression.interpreter

import com.github.javaparser.ast.expr.{Expression, SimpleName}
import example.expression.domain.Domain
import example.expression.j.TestGenerator
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait InterpreterTestGenerator extends TestGenerator {
  val domain:Domain

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(inst:domain.ExpInst, model:domain.Model) : Expression = {
    val name = inst.e.name

    val classify:SimpleName = Java(model.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")).simpleName()

    inst match {
      case lit:domain.LitInst => Java(s"new $classify$name(${lit.i.get.toString})").expression()
      case ui:domain.UnaryExpInst =>
        Java(s"new $classify$name(${convert(ui.exp, model)})").expression()
      case bi:domain.BinaryExpInst =>
        Java(s"new $classify$name(${convert(bi.left, model)}, ${convert(bi.right, model)})").expression()

      case _ =>  Java(s""" "unknown $name" """).expression()
    }
  }

}







