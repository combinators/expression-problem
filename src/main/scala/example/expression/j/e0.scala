package example.expression.j   /*DD:LD:AI*/

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.M0
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends AbstractGenerator with TestGenerator with M0 {

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => Java(s"return ${subs(litValue)};").statements
          case Add => Java(s"return ${dispatch(subs(domain.base.left),op)} + ${dispatch(subs(domain.base.right),op)};").statements()
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val a1 = new domain.BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(5.0)

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(3.0, ${dispatch(convert(a1), Eval)});
         |   assertEquals(5.0, ${dispatch(convert(lit1), Eval)});
         |}""".stripMargin).methodDeclarations()
  }
}
