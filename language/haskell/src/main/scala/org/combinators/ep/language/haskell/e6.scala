package ep.haskell    /*DD:LD:AI*/

import ep.domain._
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._

/**
  * Truly independent of the specific design solution.
  */
trait e6 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 with M3 with M4 with M5 with M6 {
  self:e0 with e1 with e2 with e3 with e4 with e5 =>
  val domain:MathDomain
  import domain._

  abstract override def typeConverter(tpe:TypeRep) : HaskellType = {
    tpe match {
      case Boolean => new HaskellType("Bool")
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Equals => scala.List[domain.Operation](domain.AsTree)
      case _ => super.dependency(op)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case Boolean => new Haskell("False")
      case _ => super.standardDefault(tpe)
    }
  }

  /**
    * We need to take action with equals operations and provide default fall-through case
    * @param op
    * @return
    */
  abstract override def requireDefault(op:domain.Operation) : Option[(Int,Haskell)] = {
    op match {
      case Equals => Some((2,standardDefault(op.returnType.get)))
      case _ => super.requireDefault(op)
    }
  }

  abstract override def logic(exp:Atomic, op:Operation): Seq[Haskell] = {
    // generate the actual body
    op match {
      case Equals =>
        exp match {
          case Lit  =>
            val value2 =  Haskell(expression(exp, litValue).getCode + "2")
            result(Haskell(s" ${expression(exp, litValue)} == $value2 "))

          case Neg  =>
            val inner2 = Haskell(expression(exp,base.inner).getCode + "2")
            result(Haskell(s" ${dispatch(expression(exp,base.inner), op, inner2)} "))

          case Add|Sub|Mult|Divd =>
            val left2 = Haskell(expression(exp, base.left).getCode + "2")
            val right2 = Haskell(expression(exp, base.right).getCode + "2")
            result(Haskell(s" ${dispatch(expression(exp, base.left), op, left2)} && ${dispatch(expression(exp, base.right), op, right2)} "))


          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  override def hunitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
    test match {
      case eb: EqualsBinaryMethodTestCase =>
        val source = NoSource()
        val full = contextDispatch(source, deltaExprOp(source, convert(eb.inst1), Equals, convert(eb.inst2)))

        if (eb.result) {
          Seq(Haskell(s"""test_v$idx = TestCase (assertBool "EqualsCheck" ($full))"""))
        } else {
          Seq(Haskell(s"""test_v$idx = TestCase (assertBool "NotEqualsCheck" (not ($full)))"""))
        }
//        if (eb.result) {
//          Seq(Haskell(s"""test_v$idx = TestCase (assertBool "EqualsCheck" (${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))}))"""))
//        } else {
//          Seq(Haskell(s"""test_v$idx = TestCase (assertBool "NotEqualsCheck" (not (${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))})))"""))
//        }
      case _ => super.hunitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M6_tests)
//    val s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
//    val s2 = new BinaryInst(Add, new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)),
//      new BinaryInst(Add, new LitInst(5.0), new LitInst(6.0)))
//    val s3 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
//
//    super.testGenerator :+ new Haskell(
//      s"""
//         |s1 = ${convert(s1)}
//         |s2 = ${convert(s2)}
//         |s3 = ${convert(s3)}
//         |test_e2_1 = TestCase (assertBool "EqualCheck" (${Equals.name} s1 s3))
//         |test_e2_2 = TestCase (assertBool "EqualCheck" (not (${Equals.name} s1 s2)))
//         |
//         |test_e2 = TestList [ TestLabel "1" test_e2_1, TestLabel "2" test_e2_2 ]
//         |
//         |main :: IO Counts
//         |main  = runTestTT test_e2
//         |""".stripMargin)
  }
}
