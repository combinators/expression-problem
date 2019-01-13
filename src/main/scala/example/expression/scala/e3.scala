package example.expression.scala  /*DD:LD:AI*/

import example.expression.domain._
import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with ScalaGenerator with TestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>
  val domain:MathDomain

   abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => result(Scala(s""" "-" + ${dispatch(subs(domain.base.inner), PrettyP)} """).expression)
          case Mult => result(Scala(s""" "(" + ${dispatch(subs(domain.base.left), PrettyP)} + "*" + ${dispatch(subs(domain.base.right), PrettyP)}  + ")" """).expression)
          case Divd => result(Scala(s""" "(" + ${dispatch(subs(domain.base.left), PrettyP)}  + "/" + ${dispatch(subs(domain.base.right), PrettyP)}  + ")" """).expression)
          case _ => super.logic(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => result(Scala(s""" - ${dispatch(subs(domain.base.inner), Eval)} """).expression)
          case Mult => result(Scala(s""" ${dispatch(subs(domain.base.left), Eval)} * ${dispatch(subs(domain.base.right), Eval)}""").expression)
          case Divd => result(Scala(s""" ${dispatch(subs(domain.base.left), Eval)} / ${dispatch(subs(domain.base.right), Eval)}""").expression)
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Stat] = {
    super.testGenerator :+ testMethod(M3_tests)
  }
}

