package example.expression.Straight

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.TestGenerator
import org.combinators.templating.twirl.Java

trait E3_Generator extends StraightGenerator with TestGenerator {
  import domain._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => Java(s"""return "-" + ${oper("exp", PrettyP)}; """).statements()
          case Mult => Java(s"""return "(" + ${oper("left", PrettyP)} + "*" + ${oper("right", PrettyP)}  + ")";""").statements()
          case Divd => Java(s"""return "(" + ${oper("left", PrettyP)}  + "/" + ${oper("right", PrettyP)}  + ")";""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => Java(s"""return - ${oper("exp", Eval)}; """).statements()
          case Mult => Java(s"""return ${oper("left", Eval)} * ${oper("right", Eval)};""").statements()
          case Divd => Java(s"""return ${oper("left", Eval)} / ${oper("right", Eval)};""").statements()
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
         |   assertEquals("-1.0", ${oper("exp1", PrettyP)});
         |   assertEquals(-1.0, ${oper("exp1", Eval)});
         |
         |   Exp  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
         |   assertEquals("((5.0/2.0)*4.0)", ${oper("exp2", PrettyP)});
         |
         |   Exp  exp3 = ${convert(d1)};
         |   assertEquals ("-5.0", ${oper("exp3", PrettyP)});
         |   Exp exp4 = ${convert(s1)};
         |   assertEquals ("-(2.0*3.0)", ${oper("exp4", PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}