package example.expression.visitor

import org.combinators.templating.twirl.Java
import expression.data.{Add, Lit}
import expression.extensions._
import expression.operations.SimplifyExpr

trait Structure  extends SemanticTypes {


//
//  // these could be loaded during combination
//  registerImpl(new Eval, Map(
//
//    new Lit -> "return e.getValue();",
//    new Add -> "return e.getLeft().accept(this) + e.getRight().accept(this);",
//    new Sub -> "return e.getLeft().accept(this) - e.getRight().accept(this);",
//    new Mult -> "return e.getLeft().accept(this) * e.getRight().accept(this);",
//    new Divd -> "return e.getLeft().accept(this) / e.getRight().accept(this);",
//    new Neg -> "return -e.getExp().accept(this);"
//  ))

//  registerImpl(new PrettyP, Map(
//    new Lit -> """return "" + e.getValue();""",
//    new Add -> """return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")";""",
//    new Sub -> """return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")";""",
//    new Mult -> """return "(" + e.getLeft().accept(this) + "*" + e.getRight().accept(this) + ")";""",
//    new Divd -> """return "(" + e.getLeft().accept(this) + "/" + e.getRight().accept(this) + ")";""",
//    new Neg -> """return "-" + e.getExp().accept(this);"""
//  ))

  val combined:String = """
          |java.util.List<Double> list = new java.util.ArrayList<Double>();
          |list.addAll(e.getLeft().accept(this));
          |list.addAll(e.getRight().accept(this));
          |return list;
        """.stripMargin

  registerImpl(new Collect, Map(
    new Lit -> """
                |java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.add(e.getValue());
                |return list;
                """.stripMargin,
    new Add -> combined,
    new Sub -> combined,
    new Mult -> combined,
    new Divd -> combined,
    new Neg -> """
                 |java.util.List<Double> list = new java.util.ArrayList<Double>();
                 |list.addAll(e.getExp().accept(this));
                 |return list;
               """.stripMargin
  ))

  addImpl(new SimplifyExpr, new Lit, Java(s"""return e;""").statements())   // nothing to simplify.
  addImpl(new SimplifyExpr, new Neg, Java(
    s"""
       |if (e.getExp().accept(new Eval()) == 0) {
       |  return new Lit(0.0);
       |} else {
       |  return e;
       |}
       """.stripMargin).statements())

  addImpl(new SimplifyExpr, new Sub, Java(
    s"""
       |if (e.getLeft().accept(new Eval()) == e.getRight().accept(new Eval())) {
       |  return new Lit(0.0);
       |} else {
       |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Mult, Java(
    s"""
       |if (e.getLeft().accept(new Eval()) == 0 || e.getRight().accept(new Eval())==0) {
       |  return new Lit(0.0);
       |} else if (e.getLeft().accept(new Eval()) == 1) {
       |  return e.getRight();
       |} else if (e.getRight().accept(new Eval()) == 1) {
       |  return e.getLeft();
       |} else {
       |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Divd, Java(
    s"""if (e.getLeft().accept(new Eval()) == 0) {
       |  return new Lit(0.0);
       |} else if (e.getRight().accept(new Eval()) == 1) {
       |  return e.getLeft();
       |} else if (e.getLeft().accept(new Eval()) == e.getRight().accept(new Eval())) {
       |  return new Lit(1.0);
       |} else if (e.getLeft().accept(new Eval()) == -e.getRight().accept(new Eval())) {
       |  return new Lit(-1.0);
       |} else {
       |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Add, Java(
    s"""
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
