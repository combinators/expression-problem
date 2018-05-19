package example.expression.visitor.e2

import example.expression.visitor.Registry
import expression.Exp
import expression.data._
import expression.extensions.{PrettyP, Sub}
import org.combinators.templating.twirl.Java

/**
 * Designed knowing this comes after E1, and thus must account for Lit, Add (E0) and Sub (E1)
 */
trait Model {

   def registerPrettyP(e: Exp, expr:com.github.javaparser.ast.expr.Expression): Unit = {

     Registry.addImpl(new PrettyP, e,
       Java(s"""return ${expr.toString};""").statements())
   }

  registerPrettyP(new Lit, Java(s""" "" + e.getValue() + "" """).expression[com.github.javaparser.ast.expr.Expression])
  registerPrettyP(new Add, Java(s""" "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")"  """).expression[com.github.javaparser.ast.expr.Expression])
  registerPrettyP(new Sub, Java(s""" "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")"  """).expression[com.github.javaparser.ast.expr.Expression])
}
