package example.expression.Pure

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import org.combinators.templating.twirl.Java

trait E3_Generator extends AbstractGenerator {
  import pure._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => Java(s"""return "-" + exp.$PRINT(); """).statements()
          case Mult => Java(s"""return "(" + left.$PRINT() + "*" + right.$PRINT() + ")";""").statements()
          case Divd => Java(s"""return "(" + left.$PRINT() + "/" + right.$PRINT() + ")";""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => Java(s"""return - exp.$EVAL(); """).statements()
          case Mult => Java(s"""return left.$EVAL() * right.$EVAL();""").statements()
          case Divd => Java(s"""return left.$EVAL() / right.$EVAL();""").statements()
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
         |   $BASE  exp1 = new Neg(new Lit(1.0));
         |   assertEquals("-1.0", exp1.$PRINT());
         |   assertEquals(-1.0, exp1.$EVAL());
         |
         |   $BASE  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
         |   assertEquals("((5.0/2.0)*4.0)", exp2.$PRINT());
         |
         |   $BASE  exp3 = ${convert(d1)};
         |   assertEquals ("-5.0", exp3.$PRINT());
         |   $BASE  exp4 = ${convert(s1)};
         |   assertEquals ("-(2.0*3.0)", exp4.$PRINT());
         |}""".stripMargin).methodDeclarations()
  }
}