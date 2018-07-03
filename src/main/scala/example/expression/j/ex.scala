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
trait ex extends AbstractGenerator with TestGenerator {
  val domain:Domain
  import domain._

  abstract override def typeGenerator(tpe:Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case JavaClass => Java(s"java.lang.Class<?>").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances
      case GetJavaClass => Java(s"""return ${getJavaClass()};""").statements()

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(model:Model): Seq[MethodDeclaration] = {
    val s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new BinaryInst(Sub, new LitInst(9.0), new LitInst(112.0))

    super.testGenerator(model.last) ++ Java(
      s"""
         |public void test() {
         |   assertEquals(${recurseOn(convert(s2, model), GetJavaClass)}, ${recurseOn(convert(s1, model), GetJavaClass)});
         |}""".stripMargin).methodDeclarations()
  }
}
