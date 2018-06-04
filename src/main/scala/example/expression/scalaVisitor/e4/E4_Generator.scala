package example.expression.scalaVisitor.e4

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.{TestGenerator, e4}
import example.expression.scalaVisitor.VisitorGenerator
import org.combinators.templating.twirl.Java

trait E4_Generator extends VisitorGenerator with TestGenerator with e4 {
  import domain._

//  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
//    tpe match {
//      case el:List => Java(s"java.util.List<${typeGenerator(el.generic)}>").tpe()
//      case _ => super.typeGenerator(tpe)
//    }
//  }
//
//  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
//    // generate the actual body
//    op match {
//      case Simplify => {
//        exp match {
//          case Lit => Java (s"return e;").statements()
//          case Add => Java(s"""
//                              |double leftVal = e.getLeft().accept(new Eval());
//                              |double rightVal = e.getRight().accept(new Eval());
//                              |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
//                              |  return new Lit(0.0);
//                              |} else if (leftVal == 0) {
//                              |  return e.getRight().accept(this);
//                              |} else if (rightVal == 0) {
//                              |  return e.getLeft().accept(this);
//                              |} else {
//                              |  return new Add(e.getLeft().accept(this), e.getRight().accept(this));
//                              |}
//                              |""".stripMargin).statements()
//          case Sub => Java(s"""
//                              |if (e.getLeft().accept(new Eval()).equals(e.getRight().accept(new Eval()))) {
//                              |  return new Lit(0.0);
//                              |} else {
//                              |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
//                              |}
//                              |""".stripMargin).statements()
//          case Mult => Java(s"""
//                               |double leftVal = e.getLeft().accept(new Eval());
//                               |double rightVal = e.getRight().accept(new Eval());
//                               |if (leftVal == 0 || rightVal == 0) {
//                               |  return new Lit(0.0);
//                               |} else if (leftVal == 1) {
//                               |  return e.getRight().accept(this);
//                               |} else if (rightVal == 1) {
//                               |  return e.getLeft().accept(this);
//                               |} else {
//                               |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
//                               |}
//                               |""".stripMargin).statements()
//          case Divd => Java(s"""
//                               |double leftVal = e.getLeft().accept(new Eval());
//                               |double rightVal = e.getRight().accept(new Eval());
//                               |if (leftVal == 0) {
//                               |  return new Lit(0.0);
//                               |} else if (rightVal == 1) {
//                               |  return e.getLeft().accept(this);
//                               |} else if (leftVal == rightVal) {
//                               |  return new Lit(1.0);
//                               |} else if (leftVal == -rightVal) {
//                               |  return new Lit(-1.0);
//                               |} else {
//                               |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
//                               |}
//                               |""".stripMargin).statements()
//          case Neg => Java(s"""
//                               |if (e.getExp().accept(new Eval()) == 0) {
//                               |  return new Lit(0.0);
//                               |} else {
//                               |  return e;
//                               |}""".stripMargin).statements()
//          case _ => super.methodBodyGenerator(exp)(op)
//        }
//      }
//
//      case Collect => {
//        val collectStatements: Seq[Statement] =
//          Java(
//            s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
//                |list.addAll(e.getLeft().accept(this));
//                |list.addAll(e.getRight().accept(this));
//                |return list;
//                |""".stripMargin).statements()
//
//        exp match {
//          case Add | Sub | Mult | Divd => collectStatements
//          case Lit => Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
//                               |list.add(e.getValue());
//                               |return list;
//                               |""".stripMargin).statements()
//          case Neg => Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
//                              |list.addAll(e.getExp().accept(this));
//                              |return list;""".stripMargin).statements()
//          case _ => super.methodBodyGenerator(exp)(op)
//        }
//      }
//      case _ => super.methodBodyGenerator(exp)(op)
//    }
//  }
//
//  abstract override def testGenerator(): Seq[MethodDeclaration] = {
//
//    // (5/7) / (7-(2*3) --> just (5/7)
//    val d1 = new BinaryInst(Divd, new LitInst(5.0), new LitInst(7.0))
//    val m1 = new BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
//    val s1 = new BinaryInst(Sub, new LitInst(7.0), m1)
//    val d2 = new BinaryInst(Divd, d1, s1)
//
//    val str = s"""
//                 |public void test() {
//                 |   Exp  exp1 = new Neg(new Lit(1.0));
//                 |   assertEquals("-1.0", ${oper("exp1", PrettyP)});
//                 |   assertEquals(-1.0, ${oper("exp1", Eval)});
//                 |
//                 |   Exp  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
//                 |   assertEquals("((5.0/2.0)*4.0)", ${oper("exp2", PrettyP)});
//                 |
//                 |   Exp  exp3 = ${convert(d2)};
//                 |   Exp  exp4 =  ${oper("exp3", Simplify)};
//                 |   Exp  exp5 = ${convert(d1)};
//                 |   assertEquals (${oper("exp5", PrettyP)}, ${oper("exp4", PrettyP)});
//                 |
//                 |   java.util.List<Double> result1 = (java.util.List<Double>) ${oper("exp3", Collect)};
//                 |   java.util.List<Double> match = new java.util.ArrayList<Double>();
//                 |   match.add(5.0);
//                 |   match.add(7.0);
//                 |   match.add(7.0);
//                 |   match.add(2.0);
//                 |   match.add(3.0);
//                 |   assertEquals(match, result1);
//                 |}""".stripMargin
//    println(">>>>" + str)
//    super.testGenerator() ++ Java(str).methodDeclarations()
//  }
}