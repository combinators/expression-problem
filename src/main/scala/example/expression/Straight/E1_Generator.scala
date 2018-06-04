package example.expression.Straight

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.TestGenerator
import org.combinators.templating.twirl.Java

trait E1_Generator extends StraightGenerator with TestGenerator {
  import domain._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Sub => Java(s"return ${oper("left", Eval)} - ${oper("right", Eval)} ;").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(): Seq[MethodDeclaration] = {
    super.testGenerator() ++ Java(
      s"""
         |public void test() {
         |   Exp  exp1 = new Sub(new Lit(1.0), new Lit(2.0));
         |   assertEquals(-1.0, ${oper("exp1", Eval)});
         |}""".stripMargin).methodDeclarations()
  }
}
