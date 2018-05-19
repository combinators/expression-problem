package example.expression.visitor.e4

import com.github.javaparser.ast.stmt.Statement
import example.expression.visitor._
import expression.Exp
import expression.data.{Add, Lit}
import expression.extensions._
import expression.operations.SimplifyExpr
import org.combinators.templating.twirl.Java

trait Model {

  def registerCollect(e: Exp, stmts:Seq[Statement]): Unit = {
    Registry.addImpl(new Collect, e, stmts)
  }

  // drawn from model
  registerCollect(new Lit, Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                    |list.add(e.getValue());
                                    |return list;
                                    |""".stripMargin).statements())

  List[Exp](new Add, new Sub, new Mult, new Divd).foreach(e =>
    registerCollect(e, Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                    |list.addAll(e.getLeft().accept(this));
                                    |list.addAll(e.getRight().accept(this));
                                    |return list;
                                    |""".stripMargin).statements()))

    registerCollect(new Neg, Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                    |list.addAll(e.getExp().accept(this));
                                    |return list;""".stripMargin).statements())


  def registerSimplify(e: Exp, stmts:Seq[Statement]): Unit = {
    Registry.addImpl(new SimplifyExpr, e, stmts)
  }

  registerSimplify(new Lit, Java(s"""return e;""").statements())

  registerSimplify(new Add, Java(s"""
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

  registerSimplify(new Sub, Java(s"""
                                    |if (e.getLeft().accept(new Eval()).equals(e.getRight().accept(new Eval()))) {
                                    |  return new Lit(0.0);
                                    |} else {
                                    |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
                                    |}
                                    |""".stripMargin).statements())

  registerSimplify(new Neg, Java(s"""
                                    |if (e.getExp().accept(new Eval()) == 0) {
                                    |  return new Lit(0.0);
                                    |} else {
                                    |  return e;
                                    |}""".stripMargin).statements())

  registerSimplify(new Mult, Java(s"""
                                     |double leftVal = e.getLeft().accept(new Eval());
                                     |double rightVal = e.getRight().accept(new Eval());
                                     |if (leftVal == 0 || rightVal == 0) {
                                     |  return new Lit(0.0);
                                     |} else if (leftVal == 1) {
                                     |  return e.getRight().accept(this);
                                     |} else if (rightVal == 1) {
                                     |  return e.getLeft().accept(this);
                                     |} else {
                                     |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
                                     |}
                                     |""".stripMargin).statements())

  registerSimplify(new Divd, Java(s"""
                                     |double leftVal = e.getLeft().accept(new Eval());
                                     |double rightVal = e.getRight().accept(new Eval());
                                     |if (leftVal == 0) {
                                     |  return new Lit(0.0);
                                     |} else if (rightVal == 1) {
                                     |  return e.getLeft().accept(this);
                                     |} else if (leftVal == rightVal) {
                                     |  return new Lit(1.0);
                                     |} else if (leftVal == -rightVal) {
                                     |  return new Lit(-1.0);
                                     |} else {
                                     |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
                                     |}
                                     |""".stripMargin).statements())


}
