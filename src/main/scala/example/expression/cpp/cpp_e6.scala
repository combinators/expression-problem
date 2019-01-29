package example.expression.cpp     /*DD:LD:AI*/

import example.expression.domain._

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

  abstract override def logic(exp: domain.Atomic, op: domain.Operation): Seq[CPPElement] = {
    val source = Source(exp, op)
    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val thatSource = NoSource()
        val leftDelta = deltaSelfOp(source, domain.AsTree)
        val rightDelta = deltaExprOp(thatSource, new CPPElement("that"), domain.AsTree) // was Independent
      val lhs: Expression = contextDispatch(source, leftDelta)
        val rhs: Expression = contextDispatch(thatSource, rightDelta)
        result(new CPPElement(s"$lhs->same($rhs)"))

      case _ => super.logic(exp, op)
    }
  }

  /**
    * Add testing capability for [[EqualsBinaryMethodTestCase]]
    */
  override def cppUnitTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
    test match {
      case eb: EqualsBinaryMethodTestCase =>
        val source = NoSource()
        val code = contextDispatch(source, deltaExprOp(source, rec_convert(eb.inst1), Equals, rec_convert(eb.inst2)))
        if (eb.result) {
          Seq(new CPPElement(s"CHECK_TRUE($code);"))
        } else {
          Seq(new CPPElement(s"CHECK_TRUE(!($code));"))
        }
      case _ =>
        super.cppUnitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M6_tests)
  }
}
