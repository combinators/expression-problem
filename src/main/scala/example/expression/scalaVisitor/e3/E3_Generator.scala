package example.expression.scalaVisitor.e3

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.{TestGenerator, e3}
import example.expression.scalaVisitor.VisitorGenerator
import org.combinators.templating.twirl.Java

trait E3_Generator extends VisitorGenerator with TestGenerator with e3 {
  import domain._

//  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
//    // generate the actual body
//    op match {
//      case Eval => {
//        exp match {
//          case Neg => Java(s"""return -1 * e.getExp().accept(this);""").statements()
//          case Mult => Java(s"""return ${oper("e.getLeft()", Eval)} * ${oper("e.getRight()", Eval)};""").statements()
//          case Divd => Java(s"""return ${oper("e.getLeft()", Eval)} / ${oper("e.getRight()", Eval)};""").statements()
//          case _ => super.methodBodyGenerator(exp)(op)
//        }
//      }
//
//      case PrettyP => {
//        exp match {
//          case Neg => Java(s"""return "-" + e.getExp().accept(this);""").statements()
//          case Mult => Java(s"""return "(" + e.getLeft().accept(this) + "*" + e.getRight().accept(this) + ")"; """).statements()
//          case Divd => Java(s"""return "(" + e.getLeft().accept(this) + "/" +  e.getRight().accept(this) + ")"; """).statements()
//          case _ => super.methodBodyGenerator(exp)(op)
//        }
//      }
//      case _ => super.methodBodyGenerator(exp)(op)
//    }
//  }
//
//  abstract override def testGenerator(): Seq[MethodDeclaration] = {
//
//
//    // (5/7) / (7-(2*3) --> just (5/7)
//    val d1 = new UnaryInst(Neg, new LitInst(5.0))
//    val m1 = new BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
//    val s1 = new UnaryInst(Neg, m1)
//
//    super.testGenerator() ++ Java(
//      s"""
//         |public void test() {
//         |   Exp  exp1 = new Neg(new Lit(1.0));
//         |   assertEquals("-1.0", exp1.accept(new Print()));
//         |   assertEquals(-1.0, exp1.accept(new Eval()));
//         |
//         |   Exp  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
//         |   assertEquals("((5.0/2.0)*4.0)", exp2.accept(new Print()));
//         |
//         |   Exp  exp3 = ${convert(d1)};
//         |   assertEquals ("-5.0", exp3.accept(new Print()));
//         |   Exp  exp4 = ${convert(s1)};
//         |   assertEquals ("-(2.0*3.0)", exp4.accept(new Print()));
//         |}""".stripMargin).methodDeclarations()
//  }
}