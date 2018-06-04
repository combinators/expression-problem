package example.expression.scalaVisitor.e0

import example.expression.j.{TestGenerator, e0}
import example.expression.scalaVisitor.VisitorGenerator


trait E0_Generator extends VisitorGenerator with TestGenerator with e0 {

//  abstract override def testGenerator(): Seq[MethodDeclaration] = {
//
//    // (5/7) / (7-(2*3) --> just (5/7)
//    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
//    val lit1 = new LitInst(3.0)
//
//    super.testGenerator() ++ Java(
//      s"""
//         |public void test() {
//         |   Exp exp1 = ${convert(a1)};
//         |   assertEquals(3.0, ${oper("exp1", Eval)});
//         |
//         |   Exp lit1 = ${convert(lit1)};
//         |   assertEquals(3.0, ${oper("lit1", Eval)});
//         |}""".stripMargin).methodDeclarations()
//  }
//
//  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
//    tpe match {
//      case Double => Java("Double").tpe()
//      case _ => super.typeGenerator(tpe)
//    }
//  }
//
//  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
//    // generate the actual body
//    op match {
//      case Eval => {
//        exp match {
//          case Lit => Java(s"return e.getValue();").statements
//          case Add => Java(s"return ${oper("e.getLeft()", Eval)} + ${oper("e.getRight()", Eval)} ;").statements()
//          case _ => super.methodBodyGenerator(exp)(op)
//        }
//      }
//
//      case _ => super.methodBodyGenerator(exp)(op)
//    }
//  }
}