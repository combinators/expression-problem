package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.math.{M0, M5, M6}
import org.combinators.ep.domain.Evolution
import org.combinators.ep.generator.OperationDependency
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Determine if structure of two Exps are equal to each other. Checking in.
  *
  * First operation that has parameter which has eExp-recursive structure
  */
trait e6 extends Evolution with DomainIndependentJavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M5 with M6 {
  self: e0 with e1 with e2 with e3 with e4 with e5 =>

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Equals => scala.List[domain.Operation](domain.AsTree)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep): com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp, op)
    op match {
      case Equals =>

        // GOAL: requesting AsTree on self produces same tree as invoking AsTree on that.
        val deltaLeft = dispatchSelf(domain.AsTree)
        val that = Java(domain.base.that.name).expression[Expression]()  //[T] needs to be here
        val deltaRight = dispatchToExpression(that, domain.AsTree)
        val lhs = contextDispatch(source, deltaLeft)
        val rhs = contextDispatch(source, deltaRight)
        result(Java(s"$lhs.same($rhs)").expression())

      case _ => super.logic(exp, op)
    }
  }

  override def junitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
      test match {
        case eb: EqualsBinaryMethodTestCase =>
          val leftBlock = toTargetLanguage(eb.inst1)
          val rightBlock = toTargetLanguage(eb.inst2)
          leftBlock.appendDependent { case Seq(leftExp) =>
            rightBlock.appendDependent { case Seq(rightExp) =>
                CodeBlockWithResultingExpressions(
                  if (eb.result) {
                    Java(s"assertTrue (${dispatch(leftExp, Equals, rightExp)});").statement
                  } else {
                    Java(s"assertFalse(${dispatch(leftExp, Equals, rightExp)});").statement
                  }
                )()
            }
          }.block
        case _ => super.junitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M6_tests)
  }
}
