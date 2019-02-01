package ep.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import ep.domain._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with JavaGenerator with JUnitTestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>
  val domain:MathDomain

   abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    op match {
      case PrettyP => {
        exp match {
          case Neg => result(Java(s""" "-" + ${dispatch(expression(exp,domain.base.inner), PrettyP)} """).expression[Expression]())
          case Mult => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)} + "*" + ${dispatch(expression(exp, domain.base.right), PrettyP)}  + ")" """).expression[Expression]())
          case Divd => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)}  + "/" + ${dispatch(expression(exp, domain.base.right), PrettyP)}  + ")" """).expression[Expression]())
          case _ => super.logic(exp, op)
        }
      }

      case Eval => {
        exp match {
          case Neg => result(Java(s" - ${dispatch(expression(exp,domain.base.inner), Eval)} ").expression[Expression]())
          case Mult => result(Java(s" ${dispatch(expression(exp, domain.base.left), Eval)} * ${dispatch(expression(exp, domain.base.right), Eval)} ").expression[Expression]())
          case Divd => result(Java(s" ${dispatch(expression(exp, domain.base.left), Eval)} / ${dispatch(expression(exp, domain.base.right), Eval)} ").expression[Expression]())
          case _ => super.logic(exp, op)
        }
      }
      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M3_tests)
  }
}
