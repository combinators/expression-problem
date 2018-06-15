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
          case Sub => Java(s"return ${recurseOn(subs("left"), Eval)} - ${recurseOn(subs("right"), Eval)} ;").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(model:Model): Seq[MethodDeclaration] = {
    val s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

    super.testGenerator(model.last) ++ Java(
      s"""
         |public void test() {
         |   assertEquals(-1.0, ${recurseOn(convert(s1, model), Eval)});
         |}""".stripMargin).methodDeclarations()
  }
}
