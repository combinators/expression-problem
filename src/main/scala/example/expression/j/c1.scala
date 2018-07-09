package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{Evolution, M2, MC1, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait c1 extends Evolution with AbstractGenerator with TestGenerator with M2 with MC1 {
  self: e0 with e1 with e2 with e3 with i1 with i2 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Inv => Java(s"""return "(1.0/" + ${recurseOn(subs(domain.base.inner), PrettyP)} + ")"; """).statements()
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val d1 = new domain.BinaryInst(Divd, new LitInst(1.0),
      new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)))
    val s2 = new domain.UnaryInst(Inv ,s1)

      super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals("(1.0/(1.0-2.0))", ${recurseOn(convert(s2), PrettyP)});
         |   assertEquals(${recurseOn(convert(d1), PrettyP)}, ${recurseOn(convert(s2), PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
