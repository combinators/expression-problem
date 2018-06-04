package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends AbstractGenerator with TestGenerator {
  val domain:Domain
  import domain._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs:Seq[Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval => {
        exp match {
          case Lit => Java(s"return ${subs(0)};").statements
          case Add => Java(s"""return ${recurseOn(subs(0),op)} + ${recurseOn(subs(1),op)};""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(): Seq[MethodDeclaration] = {

    // (5/7) / (7-(2*3) --> just (5/7)
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(3.0)

    super.testGenerator() ++ Java(
      s"""
         |public void test() {
         |   Exp exp1 = ${convert(a1)};
         |   assertEquals(3.0, ${recurseOn(Java("exp1").expression(), Eval)});
         |
         |   Exp exp2 = ${convert(lit1)};
         |   assertEquals(3.0, ${recurseOn(Java("exp2").expression(), Eval)});
         |}""".stripMargin).methodDeclarations()
  }

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }
}
