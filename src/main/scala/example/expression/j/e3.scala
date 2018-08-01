package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with AbstractGenerator with TestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>
  val domain:MathDomain

   abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => Java(s"""return "-" + ${dispatch(subs(domain.base.inner), PrettyP)}; """).statements()
          case Mult => Java(s"""return "(" + ${dispatch(subs(domain.base.left), PrettyP)} + "*" + ${dispatch(subs(domain.base.right), PrettyP)}  + ")";""").statements()
          case Divd => Java(s"""return "(" + ${dispatch(subs(domain.base.left), PrettyP)}  + "/" + ${dispatch(subs(domain.base.right), PrettyP)}  + ")";""").statements()
          case _ => super.logic(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => Java(s"""return - ${dispatch(subs(domain.base.inner), Eval)}; """).statements()
          case Mult => Java(s"""return ${dispatch(subs(domain.base.left), Eval)} * ${dispatch(subs(domain.base.right), Eval)};""").statements()
          case Divd => Java(s"""return ${dispatch(subs(domain.base.left), Eval)} / ${dispatch(subs(domain.base.right), Eval)};""").statements()
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val n1 = new domain.UnaryInst(Neg, new LitInst(1.0))

    // (5/7) / (7-(2*3) --> just (5/7)
    val d1 = new domain.UnaryInst(Neg, new LitInst(5.0))
    val m1 = new domain.BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val s1 = new domain.UnaryInst(Neg, m1)

    val m2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(4.0))

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals("-1.0", ${dispatch(convert(n1), PrettyP)});
         |   assertEquals(-1.0, ${dispatch(convert(n1), Eval)});
         |   assertEquals("((5.0/2.0)*4.0)", ${dispatch(convert(m2), PrettyP)});
         |
         |   assertEquals ("-5.0", ${dispatch(convert(d1), PrettyP)});
         |   assertEquals ("-(2.0*3.0)", ${dispatch(convert(s1), PrettyP)});
         |}""".stripMargin).methodDeclarations
  }
}

