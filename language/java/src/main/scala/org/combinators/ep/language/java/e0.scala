package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.{Evolution, Model}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances._
import org.combinators.ep.domain.math.M0
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
case class e0 extends Evolution {
  val test = JUnitTestGenerator()
  import test.langGen.{Type,Statement,dispatch,accessAttribute}
  import test.langGen.CodeBlockWithResultingExpressions
  import test.gen

  // classify e0 as fulfilling M0
  def getModel:Model = M0.getModel

  /** E0 Introduces the concept a Double and Int type, used for the 'Eval' operation. */
  def tpe(tr:TypeRep) : Type = {
    tr match {
      case TypeRep.Double => Java("Double").tpe.asInstanceOf
      case TypeRep.Int => Java("Integer").tpe.asInstanceOf
      case _ => gen.tpe(tr).asInstanceOf
    }
  }

  /**
    * E0 Introduces Double and Int values.
    *
    * Converts instance in ExistsInstance into Java value.
    *
    * Decide to use formal java.lang.Double and java.lang.Integer because of the ambiguity
    * that can exist in JUnit when dealing with primitive types and boxed types.
    */
  def instantiate(ei:InstanceRep) : CodeBlockWithResultingExpressions = {
     ei.inst match {
      case d:scala.Double => CodeBlockWithResultingExpressions(Java(s"new Double($d)").expression())
      case i:scala.Int => CodeBlockWithResultingExpressions(Java(s"new Integer($i)").expression())
      case _ => gen.instantiate(ei).asInstanceOf
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  def logic(exp:DataTypeCase, op:Operation): Seq[Statement] = {
    op match {
      case M0.Eval =>
        exp match {
          case M0.Lit => gen.toOperationResult(accessAttribute(exp, M0.litValue).asInstanceOf).asInstanceOf
          case M0.Add => gen.toOperationResult(Java(s"${dispatch(accessAttribute(exp, Attribute.left(M0.getModel)),op)} + ${dispatch(accessAttribute(exp, Attribute.right(M0.getModel)),op)}").expression()).asInstanceOf
          case _ => gen.logic(exp, op).asInstanceOf
        }

      case _ => gen.logic(exp, op).asInstanceOf
    }
  }

  // TODO: note that test.testGenerator is always Seq.empty
  def testGenerator: Seq[MethodDeclaration] = {
    test.testGenerator ++ test.testMethod(M0.M0_tests)
  }
}
