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
trait e2 extends AbstractGenerator with TestGenerator {
  val domain:MathDomain

  import domain._

  abstract override def typeConverter(tpe:TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case String => Java("String").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:Atomic)(op:Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => Java(s"""return "" + ${subs(attributes.value)} + ""; """).statements()
          case Add => Java(s"""return "(" + ${recurseOn(subs(base.left), PrettyP)} + "+" + ${recurseOn(subs(base.right), PrettyP)}+ ")";""").statements()
          case Sub => Java(s"""return "(" + ${recurseOn(subs(base.left), PrettyP)} + "-" + ${recurseOn(subs(base.right), PrettyP)} + ")";""").statements()
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new BinaryInst(Add, new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)),
                                 new BinaryInst(Add, new LitInst(5.0), new LitInst(6.0)))

      super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals("(1.0-2.0)", ${recurseOn(convert(s1), PrettyP)});
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", ${recurseOn(convert(s2), PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
