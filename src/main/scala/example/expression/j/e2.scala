package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends AbstractGenerator with TestGenerator {
  val domain:Domain

  import domain._

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case String => Java("String").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => Java(s"""return "" + ${subs(attributes.value)} + ""; """).statements()
          case Add => Java(s"""return "(" + ${recurseOn(subs(attributes.left), PrettyP)} + "+" + ${recurseOn(subs(attributes.right), PrettyP)}+ ")";""").statements()
          case Sub => Java(s"""return "(" + ${recurseOn(subs(attributes.left), PrettyP)} + "-" + ${recurseOn(subs(attributes.right), PrettyP)} + ")";""").statements()
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
         |   assertEquals("(1.0-2.0)", ${recurseOn(Java("exp1").expression(), PrettyP)});
         |
         |   Exp  exp2 = new Add(new Sub(new Lit(1.0), new Lit(2.0)), new Add(new Lit(5.0), new Lit(6.0)));
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", ${recurseOn(Java("exp2").expression(), PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
