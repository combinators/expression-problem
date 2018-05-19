package example.expression.visitor.e3

import expression.Exp
import expression.extensions.{Divd, Mult, Neg}
import org.combinators.templating.twirl.Java

/**
  * E3 development history
  */
trait Model {
  def registerEval(exp:Exp, expr:com.github.javaparser.ast.expr.Expression)
  def registerPrettyP(exp:Exp, expr:com.github.javaparser.ast.expr.Expression)

  registerEval(new Neg, Java(s"""- e.getExp().accept(this)""").expression[com.github.javaparser.ast.expr.Expression])
  registerEval(new Mult, Java(s"""e.getLeft().accept(this) * e.getRight().accept(this)""").expression[com.github.javaparser.ast.expr.Expression])
  registerEval(new Divd, Java(s"""e.getLeft().accept(this) / e.getRight().accept(this)""").expression[com.github.javaparser.ast.expr.Expression])

  registerPrettyP(new Neg, Java(s""" "-" + e.getExp().accept(this)""").expression[com.github.javaparser.ast.expr.Expression])
  registerPrettyP(new Mult, Java(s""" "(" + e.getLeft().accept(this) + "*" + e.getRight().accept(this) + ")" """).expression[com.github.javaparser.ast.expr.Expression])
  registerPrettyP(new Divd, Java(s""" "(" + e.getLeft().accept(this) + "/" +  e.getRight().accept(this) + ")" """).expression[com.github.javaparser.ast.expr.Expression])
}
