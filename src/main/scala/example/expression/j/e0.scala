package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import expression.data.Eval
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends AbstractGenerator with TestGenerator {
  val domain:Domain

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeGenerator(tpe:domain.types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Double => Java("Double").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def methodBodyGenerator(exp:domain.expressions.Exp)(op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case domain.Eval =>
        exp match {
          case domain.Lit => Java(s"return ${subs(domain.attributes.value)};").statements
          case domain.Add => Java(s"return ${recurseOn(subs(domain.base.left),op)} + ${recurseOn(subs(domain.base.right),op)};").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(model:domain.Model): Seq[MethodDeclaration] = {

    val a1 = new domain.BinaryInst(domain.Add, new domain.LitInst(1.0), new domain.LitInst(2.0))
    val lit1 = new domain.LitInst(5.0)

    super.testGenerator(model.last) ++ Java(
      s"""
         |public void test() {
         |   assertEquals(3.0, ${recurseOn(convert(a1, model), domain.Eval)});
         |   assertEquals(5.0, ${recurseOn(convert(lit1, model), domain.Eval)});
         |}""".stripMargin).methodDeclarations()
  }

}
