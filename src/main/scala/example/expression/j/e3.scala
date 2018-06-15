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
trait e3 extends AbstractGenerator with TestGenerator {
  val domain:Domain

  import domain._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => Java(s"""return "-" + ${recurseOn(subs("exp"), PrettyP)}; """).statements()
          case Mult => Java(s"""return "(" + ${recurseOn(subs("left"), PrettyP)} + "*" + ${recurseOn(subs("right"), PrettyP)}  + ")";""").statements()
          case Divd => Java(s"""return "(" + ${recurseOn(subs("left"), PrettyP)}  + "/" + ${recurseOn(subs("right"), PrettyP)}  + ")";""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => Java(s"""return - ${recurseOn(subs("exp"), Eval)}; """).statements()
          case Mult => Java(s"""return ${recurseOn(subs("left"), Eval)} * ${recurseOn(subs("right"), Eval)};""").statements()
          case Divd => Java(s"""return ${recurseOn(subs("left"), Eval)} / ${recurseOn(subs("right"), Eval)};""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }
      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(model:Model): Seq[MethodDeclaration] = {

    val n1 = new UnaryInst(Neg, new LitInst(1.0))

    // (5/7) / (7-(2*3) --> just (5/7)
    val d1 = new UnaryInst(Neg, new LitInst(5.0))
    val m1 = new BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val s1 = new UnaryInst(Neg, m1)

    val m2 = new BinaryInst(Mult, new BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(4.0))

    super.testGenerator(model.last) ++ Java(
      s"""
         |public void test() {
         |   assertEquals("-1.0", ${recurseOn(convert(n1, model), PrettyP)});
         |   assertEquals(-1.0, ${recurseOn(convert(n1, model), Eval)});
         |   assertEquals("((5.0/2.0)*4.0)", ${recurseOn(convert(m2, model), PrettyP)});
         |
         |   assertEquals ("-5.0", ${recurseOn(convert(d1, model), PrettyP)});
         |   assertEquals ("-(2.0*3.0)", ${recurseOn(convert(s1, model), PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
