package example.expression.visitor

import org.combinators.templating.twirl.Java
import expression.data.{Add, Lit}
import expression.extensions._
import expression.operations.SimplifyExpr

trait Structure  extends SemanticTypes {

  // Note: ACCEPT returns Double, which means we can't use == as comparison against two variables, but
  // can use with constants (i.e., == 0)
  // checking if x == -y is good, since it properly unboxes/boxes

  addImpl(new SimplifyExpr, new Lit, Java(s"""return e;""").statements())   // nothing to simplify.

}
