package example.expression.Straight

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.TestGenerator
import org.combinators.templating.twirl.Java

trait E2_Generator extends StraightGenerator with TestGenerator {
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
          case Lit => Java(s"""return "" + $VALUE + ""; """).statements()
          case Add => Java(s"""return "(" + ${oper("left", PrettyP)} + "+" + ${oper("right", PrettyP)}+ ")";""").statements()
          case Sub => Java(s"""return "(" + ${oper("left", PrettyP)} + "-" + ${oper("right", PrettyP)} + ")";""").statements()
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
         |   assertEquals("(1.0-2.0)", ${oper("exp1", PrettyP)});
         |
         |   Exp  exp2 = new Add(new Sub(new Lit(1.0), new Lit(2.0)), new Add(new Lit(5.0), new Lit(6.0)));
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", ${oper("exp2", PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
