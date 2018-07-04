package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{Domain, MergedDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait m1 extends AbstractGenerator with TestGenerator {
  val domain:MergedDomain

  abstract override def logic(exp:domain.subtypes.Exp)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case domain.PrettyP =>
        exp match {
          case domain.Inv => Java(s"""return "(1.0/" + ${recurseOn(subs(domain.base.exp), domain.PrettyP)} + ")"; """).statements()
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new domain.BinaryInst(domain.Sub, new domain.LitInst(1.0), new domain.LitInst(2.0))
    val d1 = new domain.BinaryInst(domain.Divd, new domain.LitInst(1.0),
      new domain.BinaryInst(domain.Sub, new domain.LitInst(1.0), new domain.LitInst(2.0)))
    val s2 = new domain.UnaryInst(domain.Inv ,s1)

      super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals("(1.0/(1.0-2.0))", ${recurseOn(convert(s2), domain.PrettyP)});
         |   assertEquals(${recurseOn(convert(d1), domain.PrettyP)}, ${recurseOn(convert(s2), domain.PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
