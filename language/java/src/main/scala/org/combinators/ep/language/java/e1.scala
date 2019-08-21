package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.{Evolution, Model}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.math.{M0, M1}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
case class e1(last:e0) extends Evolution {
  val test = JUnitTestGenerator()
  import test.langGen.{Type,Statement,dispatch,accessAttribute}
  import test.gen

  // classify e0 as fulfilling M1
  def getModel:Model = M1.getModel

  def logic(exp:DataTypeCase, op:Operation): Seq[Statement] = {
    op match {
      case M0.Eval =>
        exp match {
          case M1.Sub => gen.toOperationResult(Java(s"${dispatch(accessAttribute(exp, Attribute.left(M0.getModel)),op)} - ${dispatch(accessAttribute(exp, Attribute.right(M0.getModel)),op)}").expression()).asInstanceOf
          case _ => gen.logic(exp, op).asInstanceOf
        }

      case _ => gen.logic(exp, op).asInstanceOf
    }
  }

  def testGenerator: Seq[MethodDeclaration] = {
    last.testGenerator ++ test.testMethod(M1.M1_tests)
  }
}
