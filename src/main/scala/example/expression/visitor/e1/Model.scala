package example.expression.visitor.e1

import expression.Exp
import expression.extensions.Sub
import org.combinators.templating.twirl.Java

trait Model {

  def registerEval(exp:Exp, expr:com.github.javaparser.ast.expr.Expression)

  registerEval(new Sub, Java(s"""e.getLeft().accept(this) - e.getRight().accept(this)""").expression[com.github.javaparser.ast.expr.Expression])
}
