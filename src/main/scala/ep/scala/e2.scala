package ep.scala    /*DD:LD:AI*/

import ep.domain.{Evolution, M0, M2, MathDomain}

import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with ScalaGenerator with TestGenerator with M0 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case String => Type.Name("String")
      case _ => super.typeConverter(tpe)
    }
  }

  /** For developing test cases with strings, must convert expected value into a Java string expression. */
  abstract override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Seq[Stat]) => Seq[Stat] = continue => {
      test.expect._1 match {
      case String => continue (Scala("\"" + test.expect._2.toString + "\"").expression)
      case _ => super.expected(test, id) (continue)
    }
  }

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Scala(s""" "" + ${expression(exp,litValue)} + "" """).expression)
          case Add => result(Scala(s""" "(" + ${dispatch(expression(exp,domain.base.left), PrettyP)} + "+" + ${dispatch(expression(exp,domain.base.right), PrettyP)}+ ")" """).expression)
          case Sub => result(Scala(s""" "(" + ${dispatch(expression(exp,domain.base.left), PrettyP)} + "-" + ${dispatch(expression(exp,domain.base.right), PrettyP)} + ")" """).expression)
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M2_tests)
  }
}
