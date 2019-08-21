package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances._
import org.combinators.ep.domain.{Evolution, Model}
import org.combinators.ep.domain.math._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
case class e2(last:e1) extends Evolution {
  val test = JUnitTestGenerator()
  import test.langGen.{Type,Statement,dispatch,accessAttribute}
  import test.langGen.CodeBlockWithResultingExpressions
  import test.gen

  // classify e0 as fulfilling M1
  def getModel:Model = M2.getModel

  /** E2 Introduces the concept a String type, used for the 'PrettyPrint' operation. */
  def typeConverter(tpe:TypeRep) : Type = {
    tpe match {
      case TypeRep.String => Java("String").tpe().asInstanceOf
      case _ => gen.tpe(tpe).asInstanceOf
    }
  }

  /** E2 Introduces String values. */
  def instantiate(ei:InstanceRep) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case s:String => CodeBlockWithResultingExpressions(new com.github.javaparser.ast.expr.StringLiteralExpr(s).asInstanceOf)
      case _ => gen.instantiate(ei).asInstanceOf
    }
  }

  def logic(exp:DataTypeCase, op:Operation): Seq[Statement] = {
    op match {
      case M2.PrettyP =>
        exp match {
          case M0.Lit => gen.toOperationResult(Java(s""" "" + ${accessAttribute(exp, M0.litValue)} """).expression()).asInstanceOf
          case M0.Add => gen.toOperationResult(Java(s""" "(" + ${dispatch(accessAttribute(exp, Attribute.left(M0.getModel)),op)} + "+" + ${dispatch(accessAttribute(exp, Attribute.right(M0.getModel)),op)} + ")" """).expression()).asInstanceOf
          case M1.Sub => gen.toOperationResult(Java(s""" "(" + ${dispatch(accessAttribute(exp, Attribute.left(M0.getModel)),op)} + "-" + ${dispatch(accessAttribute(exp, Attribute.right(M0.getModel)),op)} + ")" """).expression()).asInstanceOf
          case _ => last.logic(exp, op).asInstanceOf
        }

      case _ => last.logic(exp, op).asInstanceOf
    }
  }

  def testGenerator: Seq[MethodDeclaration] = {
    last.testGenerator ++ test.testMethod(M2.M2_tests)
  }
}
