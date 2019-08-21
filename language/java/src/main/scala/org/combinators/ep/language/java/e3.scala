package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with DomainIndependentJavaGenerator with JUnitTestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>

   abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    op match {
      case PrettyP => {
        exp match {
          case Neg => result(Java(s""" "-" + ${dispatch(expression(exp,domain.base.inner), PrettyP)} """).expression())
          case Mult => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)} + "*" + ${dispatch(expression(exp, domain.base.right), PrettyP)}  + ")" """).expression())
          case Divd => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)}  + "/" + ${dispatch(expression(exp, domain.base.right), PrettyP)}  + ")" """).expression())
          case _ => super.logic(exp, op)
        }
      }

      case Eval => {
        exp match {
          case Neg => result(Java(s" - ${dispatch(expression(exp,domain.base.inner), Eval)} ").expression())
          case Mult => result(Java(s" ${dispatch(expression(exp, domain.base.left), Eval)} * ${dispatch(expression(exp, domain.base.right), Eval)} ").expression())
          case Divd => result(Java(s" ${dispatch(expression(exp, domain.base.left), Eval)} / ${dispatch(expression(exp, domain.base.right), Eval)} ").expression())
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

