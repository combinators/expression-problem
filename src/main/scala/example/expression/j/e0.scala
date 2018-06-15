package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends AbstractGenerator with TestGenerator {
  val domain:Domain
  import domain._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval => {
        exp match {
          case Lit => Java(s"return ${subs(attributes.value)};").statements
          case Add => Java(s"return ${recurseOn(subs(attributes.left),op)} + ${recurseOn(subs(attributes.right),op)};").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(model:Model): Seq[MethodDeclaration] = {

    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(5.0)

    super.testGenerator(model.last) ++ Java(
      s"""
         |public void test() {
         |   assertEquals(3.0, ${recurseOn(convert(a1, model), Eval)});
         |   assertEquals(5.0, ${recurseOn(convert(lit1, model), Eval)});
         |}""".stripMargin).methodDeclarations()
  }

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }
}
