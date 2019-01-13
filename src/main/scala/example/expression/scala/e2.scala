package example.expression.scala    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M2, MathDomain}
import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with ScalaGenerator with TestGenerator with M2 {
  self:e0 with e1 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case String => Type.Name("String")
      case _ => super.typeConverter(tpe)
    }
  }

  /** For developing test cases with strings, must convert expected value into a Java string expression. */
  abstract override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Stat) => Stat = continue => {
      test.expect._1 match {
      case String => continue (Scala("\"" + test.expect._2.toString + "\"").expression)
      case _ => super.expected(test, id) (continue)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Scala(s""" "" + ${subs(litValue)} + "" """).expression)
          case Add => result(Scala(s""" "(" + ${dispatch(subs(domain.base.left), PrettyP)} + "+" + ${dispatch(subs(domain.base.right), PrettyP)}+ ")" """).expression)
          case Sub => result(Scala(s""" "(" + ${dispatch(subs(domain.base.left), PrettyP)} + "-" + ${dispatch(subs(domain.base.right), PrettyP)} + ")" """).expression)
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Stat] = {
    super.testGenerator :+ testMethod(M2_tests)
  }
}
