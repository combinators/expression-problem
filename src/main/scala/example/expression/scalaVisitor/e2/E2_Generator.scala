package example.expression.scalaVisitor.e2

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.TestGenerator
import example.expression.scalaVisitor.VisitorGenerator
import org.combinators.templating.twirl.Java

trait E2_Generator extends VisitorGenerator with TestGenerator {
  import domain._

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case String => Java("String").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Lit => Java(s"""return "" + e.getValue()+ ""; """).statements()
          case Add => Java(s"""return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")"; """).statements()
          case Sub => Java(s"""return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")"; """).statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(): Seq[MethodDeclaration] = {
    super.testGenerator() ++ Java(
      s"""
         |public void test() {
         |   Exp  exp1 = new Sub(new Lit(1.0), new Lit(2.0));
         |   assertEquals("(1.0-2.0)", exp1.accept (new Print()));
         |
         |   Exp exp2 = new Add(new Sub(new Lit(1.0), new Lit(2.0)), new Add(new Lit(5.0), new Lit(6.0)));
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", exp2.accept (new Print()));
         |}""".stripMargin).methodDeclarations()
  }
}
