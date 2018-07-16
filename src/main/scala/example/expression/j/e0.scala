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
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[Type] = None) : Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[Statement] = {
    val atts:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => Java(s"return ${atts(litValue)};").statements
          case Add => Java(s"return ${dispatch(atts(base.left),op)} + ${dispatch(atts(base.right),op)};").statements
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(5.0)

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(3.0, ${dispatch(convert(a1), Eval)});
         |   assertEquals(5.0, ${dispatch(convert(lit1), Eval)});
         |}""".stripMargin).methodDeclarations
  }
}
