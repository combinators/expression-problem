package example.expression.visitor

import org.combinators.templating.twirl.Java
import expression.{Exp, Operation}
import expression.data.{Add, Eval, Lit}
import expression.extensions.{Collect, Neg, PrettyP, Sub}
import expression.operations.SimplifyAdd

trait Structure extends SemanticTypes {

  def registerImpl (op:Operation, map:Map[Exp,String]): Unit = {
    map.keys.foreach {
      key =>
        addImpl(op, key, Java(map(key)).statements())
    }
  }

  registerImpl(new Eval, Map(
    new Lit -> "return e.getValue();",
    new Add -> "return e.getLeft().accept(this) + e.getRight().accept(this);",
    new Sub -> "return e.getLeft().accept(this) - e.getRight().accept(this);",
    new Neg -> "return -e.getExp().accept(this);"
  ))

  registerImpl(new PrettyP, Map(
    new Lit -> """return "" + e.getValue();""",
    new Add -> """return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")";""",
    new Sub -> """return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")";""",
    new Neg -> """return "-" + e.getExp().accept(this);"""
  ))

  val combined:String = """
          |java.util.List<Integer> list = new java.util.ArrayList<Integer>();
          |list.addAll(e.getLeft().accept(this));
          |list.addAll(e.getRight().accept(this));
          |return list;
        """.stripMargin

  registerImpl(new Collect, Map(
    new Lit -> """
                |java.util.List<Integer> list = new java.util.ArrayList<Integer>();
                |list.add(e.getValue());
                |return list;
                """.stripMargin,
    new Add -> combined,
    new Sub -> combined,
    new Neg -> """
                 |java.util.List<Integer> list = new java.util.ArrayList<Integer>();
                 |list.addAll(e.getExp().accept(this));
                 |return list;
               """.stripMargin
  ))

  addImpl(new SimplifyAdd, new Lit, Java(s"""return e;""").statements())   // nothing to simplify.
  addImpl(new SimplifyAdd, new Neg, Java(
    s"""
       |if (e.getExp().accept(new Eval()) == 0) {
       |  return new Lit(0);
       |} else {
       |  return e;
       |}
       """.stripMargin).statements())

  addImpl(new SimplifyAdd, new Sub, Java(
    s"""
       |if (e.getLeft().accept(new Eval()) == e.getRight().accept(new Eval())) {
       |  return new Lit(0);
       |} else {
       |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyAdd, new Add, Java(
    s"""
       |int leftVal = e.getLeft().accept(new Eval());
       |int rightVal = e.getRight().accept(new Eval());
       |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
       |  return new Lit(0);
       |} else if (leftVal == 0) {
       |  return e.getRight().accept(this);
       |} else if (rightVal == 0) {
       |  return e.getLeft().accept(this);
       |} else {
       |  return new Add(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())


}
