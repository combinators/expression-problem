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

  abstract override def testGenerator(): Seq[MethodDeclaration] = {


    // (5/7) / (7-(2*3) --> just (5/7)
    val d1 = new UnaryInst(Neg, new LitInst(5.0))
    val m1 = new BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val s1 = new UnaryInst(Neg, m1)

    super.testGenerator() ++ Java(
      s"""
         |public void test() {
         |   Exp  exp1 = new Neg(new Lit(1.0));
         |   assertEquals("-1.0", ${recurseOn(Java("exp1").expression(), PrettyP)});
         |   assertEquals(-1.0, ${recurseOn(Java("exp1").expression(), Eval)});
         |
         |   Exp  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
         |   assertEquals("((5.0/2.0)*4.0)", ${recurseOn(Java("exp2").expression(), PrettyP)});
         |
         |   Exp  exp3 = ${convert(d1)};
         |   assertEquals ("-5.0", ${recurseOn(Java("exp3").expression(), PrettyP)});
         |   Exp exp4 = ${convert(s1)};
         |   assertEquals ("-(2.0*3.0)", ${recurseOn(Java("exp4").expression(), PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
