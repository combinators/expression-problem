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
trait e1 extends AbstractGenerator with TestGenerator {
  val domain:Domain
  import domain._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Sub => Java(s"return ${recurseOn(subs(0), Eval)} - ${recurseOn(subs(1), Eval)} ;").statements()
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
         |   assertEquals(-1.0, ${recurseOn(Java("exp1").expression(), Eval)});
         |}""".stripMargin).methodDeclarations()
  }
}
