package example.expression.Pure

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import org.combinators.templating.twirl.Java

trait E2_Generator extends AbstractGenerator {
  import pure._

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
          case Add => Java(s"""return "(" + left.$PRINT() + "+" + right.$PRINT() + ")";""").statements()
          case Sub => Java(s"""return "(" + left.$PRINT() + "-" + right.$PRINT() + ")";""").statements()
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
         |   $BASE  exp1 = new Sub(new Lit(1.0), new Lit(2.0));
         |   assertEquals("(1.0-2.0)", exp1.$PRINT());
         |
         |   $BASE  exp2 = new Add(new Sub(new Lit(1.0), new Lit(2.0)), new Add(new Lit(5.0), new Lit(6.0)));
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", exp2.$PRINT());
         |}""".stripMargin).methodDeclarations()
  }
}
