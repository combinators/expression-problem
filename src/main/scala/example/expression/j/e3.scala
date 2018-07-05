package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.MathDomain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends AbstractGenerator with TestGenerator {
  val domain:MathDomain

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    
    // generate the actual body
    op match {
      case domain.PrettyP => {
        exp match {
          case domain.Neg => Java(s"""return "-" + ${recurseOn(subs(domain.base.inner), domain.PrettyP)}; """).statements()
          case domain.Mult => Java(s"""return "(" + ${recurseOn(subs(domain.base.left), domain.PrettyP)} + "*" + ${recurseOn(subs(domain.base.right), domain.PrettyP)}  + ")";""").statements()
          case domain.Divd => Java(s"""return "(" + ${recurseOn(subs(domain.base.left), domain.PrettyP)}  + "/" + ${recurseOn(subs(domain.base.right), domain.PrettyP)}  + ")";""").statements()
          case _ => super.logic(exp)(op)
        }
      }

      case domain.Eval => {
        exp match {
          case domain.Neg => Java(s"""return - ${recurseOn(subs(domain.base.inner), domain.Eval)}; """).statements()
          case domain.Mult => Java(s"""return ${recurseOn(subs(domain.base.left), domain.Eval)} * ${recurseOn(subs(domain.base.right), domain.Eval)};""").statements()
          case domain.Divd => Java(s"""return ${recurseOn(subs(domain.base.left), domain.Eval)} / ${recurseOn(subs(domain.base.right), domain.Eval)};""").statements()
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val n1 = new domain.UnaryInst(domain.Neg, new domain.LitInst(1.0))

    // (5/7) / (7-(2*3) --> just (5/7)
    val d1 = new domain.UnaryInst(domain.Neg, new domain.LitInst(5.0))
    val m1 = new domain.BinaryInst(domain.Mult, new domain.LitInst(2.0), new domain.LitInst(3.0))
    val s1 = new domain.UnaryInst(domain.Neg, m1)

    val m2 = new domain.BinaryInst(domain.Mult, new domain.BinaryInst (domain.Divd, new domain.LitInst(5.0),  new domain.LitInst(2.0)), new domain.LitInst(4.0))

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals("-1.0", ${recurseOn(convert(n1), domain.PrettyP)});
         |   assertEquals(-1.0, ${recurseOn(convert(n1), domain.Eval)});
         |   assertEquals("((5.0/2.0)*4.0)", ${recurseOn(convert(m2), domain.PrettyP)});
         |
         |   assertEquals ("-5.0", ${recurseOn(convert(d1), domain.PrettyP)});
         |   assertEquals ("-(2.0*3.0)", ${recurseOn(convert(s1), domain.PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
