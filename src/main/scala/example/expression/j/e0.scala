package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.MathDomain
import expression.data.Eval
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends AbstractGenerator with TestGenerator {
  val domain:MathDomain

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Double => Java("Double").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case domain.Eval =>
        exp match {
          case domain.Lit => Java(s"return ${subs(domain.attributes.value)};").statements
          case domain.Add => Java(s"return ${recurseOn(subs(domain.base.left),op)} + ${recurseOn(subs(domain.base.right),op)};").statements()
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val a1 = new domain.BinaryInst(domain.Add, new domain.LitInst(1.0), new domain.LitInst(2.0))
    val lit1 = new domain.LitInst(5.0)

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(3.0, ${recurseOn(convert(a1), domain.Eval)});
         |   assertEquals(5.0, ${recurseOn(convert(lit1), domain.Eval)});
         |}""".stripMargin).methodDeclarations()
  }

}
