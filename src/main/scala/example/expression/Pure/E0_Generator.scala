package example.expression.Pure

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import org.combinators.templating.twirl.Java


trait E0_Generator extends AbstractGenerator {
  import pure._

  abstract override def testGenerator(): Seq[MethodDeclaration] = {
    super.testGenerator() ++ Java(
      s"""
         |public void test() {
         |   $BASE exp1 = new Add(new Lit(1.0), new Lit(2.0));
         |   assertEquals(3.0, exp1.$EVAL());
         |
         |   Lit lit1 = new Lit(3.0);
         |   assertEquals(3.0, lit1.$EVAL());
         |}""".stripMargin).methodDeclarations()
  }

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Eval => {
        exp match {
          case Lit => Java(s"return $VALUE;").statements
          case Add => Java(s"return left.$EVAL() + right.$EVAL();").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }
}