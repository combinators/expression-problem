package example.expression.j   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.M0
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends JavaGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tr:TypeRep, covariantReplacement:Option[Type] = None) : Type = {
    tr match {
      case Double => Java("Double").tpe
      case Int => Java("Integer").tpe
      case _ => super.typeConverter(tr, covariantReplacement)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[Statement] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => Java(s"return ${atts(litValue)};").statements
          case Add => Java(s"return ${dispatch(atts(base.left),op)} + ${dispatch(atts(base.right),op)};").statements
          case _ => super.logic(exp)(op)
        }

        // all future EXP sub-types can simply return hashcode.
      case Identifier => Java(s"return ${exp.hashCode()};").statements

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
