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
  addImpl(new SimplifyExpr, new Neg,
    Java(s"""
       |if (e.getExp().accept(new Eval()) == 0) {
       |  return new Lit(0.0);
       |} else {
       |  return e;
       |}
       """.stripMargin).statements())

  addImpl(new SimplifyExpr, new Sub,
    Java(s"""
       |if (e.getLeft().accept(new Eval()).equals(e.getRight().accept(new Eval()))) {
       |  return new Lit(0.0);
       |} else {
       |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Mult,
    Java(s"""
       |double leftVal = e.getLeft().accept(new Eval());
       |double rightVal = e.getRight().accept(new Eval());
       |if (leftVal == 0 || rightVal == 0) {
       |  return new Lit(0.0);
       |} else if (leftVal == 1) {
       |  return e.getRight();
       |} else if (rightVal == 1) {
       |  return e.getLeft();
       |} else {
       |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Divd,
    Java(s"""
       |double leftVal = e.getLeft().accept(new Eval());
       |double rightVal = e.getRight().accept(new Eval());
       |if (leftVal == 0) {
       |  return new Lit(0.0);
       |} else if (rightVal == 1) {
       |  return e.getLeft();
       |} else if (leftVal == rightVal) {
       |  return new Lit(1.0);
       |} else if (leftVal == -rightVal) {
       |  return new Lit(-1.0);
       |} else {
       |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Add,
    Java(s"""
       |double leftVal = e.getLeft().accept(new Eval());
       |double rightVal = e.getRight().accept(new Eval());
       |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
       |  return new Lit(0.0);
       |} else if (leftVal == 0) {
       |  return e.getRight().accept(this);
       |} else if (rightVal == 0) {
       |  return e.getLeft().accept(this);
       |} else {
       |  return new Add(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())
}
