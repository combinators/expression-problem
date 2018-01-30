package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import org.combinators.templating.twirl.Java
import expression.{Exp, Operation}
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.operations.SimplifyExpr
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Type

import scala.collection.JavaConverters._

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
    new Mult -> "return e.getLeft().accept(this) * e.getRight().accept(this);",
    new Divd -> "return e.getLeft().accept(this) / e.getRight().accept(this);",
    new Neg -> "return -e.getExp().accept(this);"
  ))

  registerImpl(new PrettyP, Map(
    new Lit -> """return "" + e.getValue();""",
    new Add -> """return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")";""",
    new Sub -> """return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")";""",
    new Mult -> """return "(" + e.getLeft().accept(this) + "*" + e.getRight().accept(this) + ")";""",
    new Divd -> """return "(" + e.getLeft().accept(this) + "/" + e.getRight().accept(this) + ")";""",
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
    new Mult -> combined,
    new Divd -> combined,
    new Neg -> """
                 |java.util.List<Integer> list = new java.util.ArrayList<Integer>();
                 |list.addAll(e.getExp().accept(this));
                 |return list;
               """.stripMargin
  ))

  addImpl(new SimplifyExpr, new Lit, Java(s"""return e;""").statements())   // nothing to simplify.
  addImpl(new SimplifyExpr, new Neg, Java(
    s"""
       |if (e.getExp().accept(new Eval()) == 0) {
       |  return new Lit(0);
       |} else {
       |  return e;
       |}
       """.stripMargin).statements())

  addImpl(new SimplifyExpr, new Sub, Java(
    s"""
       |if (e.getLeft().accept(new Eval()) == e.getRight().accept(new Eval())) {
       |  return new Lit(0);
       |} else {
       |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Mult, Java(
    s"""
       |if (e.getLeft().accept(new Eval()) ==0 || e.getRight().accept(new Eval())==0) {
       |  return new Lit(0);
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
       |  return new Lit(0);
       |}else if (e.getRight().accept(new Eval()) == 1) {
       |  return e.getLeft();
       |} else {
       |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
       |}
       |""".stripMargin).statements())

  addImpl(new SimplifyExpr, new Add, Java(
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

  // sample Driver
  @combinator object Driver {
    def apply:CompilationUnit = Java(s"""
        |package expression;
        |
        |public class Driver {
        |  public static void main(String[] args) {
        |        System.out.println("======Add======");
        |        Add add = new Add(new Lit(7), new Lit(4));
        |        System.out.println(add.accept(new Eval()));
        |        System.out.println("======Sub======");
        |        Sub sub = new Sub(new Lit(7), new Lit(4));
        |        System.out.println(sub.accept(new Eval()));
        |        System.out.println("======Mult======");
        |        Mult mult = new Mult(new Lit(7), new Lit(4));
        |        System.out.println(mult.accept(new Eval()));
        |         System.out.println("======Divd======");
        |        Divd divd = new Divd(new Lit(7), new Lit(4));
        |        System.out.println(divd.accept(new Eval()));
        |        System.out.println("======Print======");
        |        Exp expr = new Mult(new Lit(5), new Add(new Lit(3), new Lit(-3)));
        |        System.out.println(expr.accept(new PrettyP()));
        |        Exp simpler = expr.accept(new SimplifyExpr());
        |        System.out.println(simpler.accept(new PrettyP()));
        |
        |  }
        |}""".stripMargin).compilationUnit()

    val semanticType:Type = driver
  }


}
