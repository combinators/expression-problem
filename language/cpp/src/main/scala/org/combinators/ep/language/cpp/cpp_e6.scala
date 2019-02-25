package org.combinators.ep.language.cpp     /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e6 extends Evolution with CPPGenerator with CPPBinaryMethod with TestGenerator with M0 with M1 with M2 with M3 with M4 with M5 with M6 {
  self: cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5 =>

  import domain._

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Equals => scala.List[domain.Operation](AsTree)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe: domain.TypeRep): CPPType = {
    tpe match {
      case Boolean => new CPPType("bool")
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp: domain.DataType, op: domain.Operation): Seq[CPPStatement] = {
    val source = Source(exp, op)
    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val thatSource = NoSource
        val deltaLeft = deltaSelfOp(source, domain.AsTree)
        val that = new CPPExpression(domain.base.that.name)
        val deltaRight = deltaExprOp(thatSource, that, domain.AsTree)
        val lhs = contextDispatch(source, deltaLeft)
        val rhs = contextDispatch(thatSource, deltaRight)
        result(new CPPExpression(s"$lhs->same($rhs)"))

      case _ => super.logic(exp, op)
    }
  }

  override def cppUnitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
    test match {
      case eb: EqualsBinaryMethodTestCase =>
        val leftBlock = toTargetLanguage(eb.inst1)
        val rightBlock = toTargetLanguage(eb.inst2)
        leftBlock.appendDependent { case Seq(leftExp) =>
          rightBlock.appendDependent { case Seq(rightExp) =>
            CodeBlockWithResultingExpressions(
              if (eb.result) {
                val delta = deltaExprOp(NoSource, leftExp, Equals, rightExp)
                val code = contextDispatch (NoSource, delta)
                new CPPStatement(s"CHECK_TRUE ($code);")
              } else {
                val delta = deltaExprOp(NoSource, leftExp, Equals, rightExp)
                val code = contextDispatch (NoSource, delta)
                new CPPStatement(s"CHECK_FALSE($code);")
              }
            )()
          }
        }.block
      case _ => super.cppUnitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M6_tests)
  }
}
