package example.expression.j

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.MathDomain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends AbstractGenerator with TestGenerator {
  val domain:MathDomain

//  import domain._

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.String => Java("String").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case domain.PrettyP =>
        exp match {
          case domain.Lit => Java(s"""return "" + ${subs(domain.attributes.value)} + ""; """).statements()
          case domain.Add => Java(s"""return "(" + ${recurseOn(subs(domain.base.left), domain.PrettyP)} + "+" + ${recurseOn(subs(domain.base.right), domain.PrettyP)}+ ")";""").statements()
          case domain.Sub => Java(s"""return "(" + ${recurseOn(subs(domain.base.left), domain.PrettyP)} + "-" + ${recurseOn(subs(domain.base.right), domain.PrettyP)} + ")";""").statements()
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new domain.BinaryInst(domain.Sub, new domain.LitInst(1.0), new domain.LitInst(2.0))
    val s2 = new domain.BinaryInst(domain.Add, new domain.BinaryInst(domain.Sub, new domain.LitInst(1.0), new domain.LitInst(2.0)),
                                 new domain.BinaryInst(domain.Add, new domain.LitInst(5.0), new domain.LitInst(6.0)))

      super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals("(1.0-2.0)", ${recurseOn(convert(s1), domain.PrettyP)});
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", ${recurseOn(convert(s2), domain.PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
