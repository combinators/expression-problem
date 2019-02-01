package ep.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import ep.domain.{Evolution, M0, M2, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with JavaGenerator with JUnitTestGenerator with M0 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case String => Java("String").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** For developing test cases with strings, must convert expected value into a Java string expression. */
  abstract override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
      test.expect._1 match {
      case String => continue (Java("\"" + test.expect._2.toString + "\"").expression[Expression])
      case _ => super.expected(test, id) (continue)
    }
  }

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Java(s""" "" + ${expression(exp,litValue)} + "" """).expression[Expression]())
          case Add => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)} + "+" + ${dispatch(expression(exp, domain.base.right), PrettyP)}+ ")" """).expression[Expression]())
          case Sub => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)} + "-" + ${dispatch(expression(exp, domain.base.right), PrettyP)} + ")" """).expression[Expression]())
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M2_tests)
  }
}