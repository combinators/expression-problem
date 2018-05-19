package example.expression.visitor.e0

import example.expression.visitor._
import expression.data.{Add, Eval, Lit}
import expression.instances.UnitSuite
import expression.{DomainModel, Exp}
import org.combinators.templating.twirl.Java

/**
  * Each trait is stand alone, and woven into the final repository.
  */
trait Model {

  def registerEval(e: Exp, expr:com.github.javaparser.ast.expr.Expression): Unit = {
    Registry.addImpl(new Eval, e,
      Java(s"""return ${expr.toString};""").statements())
  }

  // drawn from model
  registerEval(new Lit, Java(s"""e.getValue()""").expression[com.github.javaparser.ast.expr.Expression])
  registerEval(new Add, Java(s"""e.getLeft().accept(this) + e.getRight().accept(this)""").expression[com.github.javaparser.ast.expr.Expression])

}
