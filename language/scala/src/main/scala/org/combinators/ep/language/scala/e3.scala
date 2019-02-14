package org.combinators.ep.language.scala   /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._

import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with ScalaGenerator with TestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>
  val domain:MathDomain

   abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    op match {
      case PrettyP => {
        exp match {
          case Neg => result(Scala(s""" "-" + ${dispatch(expression(exp, domain.base.inner), PrettyP)} """).expression)
          case Mult => result(Scala(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)} + "*" + ${dispatch(expression(exp, domain.base.right), PrettyP)}  + ")" """).expression)
          case Divd => result(Scala(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)}  + "/" + ${dispatch(expression(exp, domain.base.right), PrettyP)}  + ")" """).expression)
          case _ => super.logic(exp, op)
        }
      }

      case Eval => {
        exp match {
          case Neg => result(Scala(s""" - ${dispatch(expression(exp, domain.base.inner), Eval)} """).expression)
          case Mult => result(Scala(s""" ${dispatch(expression(exp, domain.base.left), Eval)} * ${dispatch(expression(exp, domain.base.right), Eval)}""").expression)
          case Divd => result(Scala(s""" ${dispatch(expression(exp, domain.base.left), Eval)} / ${dispatch(expression(exp, domain.base.right), Eval)}""").expression)
          case _ => super.logic(exp, op)
        }
      }
      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M3_tests)
  }
}

