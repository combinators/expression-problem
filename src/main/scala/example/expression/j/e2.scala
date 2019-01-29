package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{Evolution, M2, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with JavaGenerator with JUnitTestGenerator with M2 {
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
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Java(s""" "" + ${subs(litValue)} + "" """).expression[Expression]())
          case Add => result(Java(s""" "(" + ${dispatch(subs(domain.base.left), PrettyP)} + "+" + ${dispatch(subs(domain.base.right), PrettyP)}+ ")" """).expression[Expression]())
          case Sub => result(Java(s""" "(" + ${dispatch(subs(domain.base.left), PrettyP)} + "-" + ${dispatch(subs(domain.base.right), PrettyP)} + ")" """).expression[Expression]())
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M2_tests)
  }
}
