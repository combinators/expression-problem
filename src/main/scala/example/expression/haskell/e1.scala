package example.expression.haskell

/*DD:LD:AI*/

import example.expression.domain.{Evolution, M1, MathDomain}

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e1 extends Evolution with AbstractGenerator with TestGenerator with M1 {
  self:e0 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Haskell] = {
    val subs = subExpressions(exp)
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Sub => Seq(new Haskell(s"a1 - a2"))
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

    val exp_s1:String = expand("s1_", s1).map(line => s"$line :: GeneralExpr").mkString("\n")


    super.testGenerator :+ new Haskell(
      s"""
         |$exp_s1
         |-- for some reason, can't type in "-1.0" plain, but must make it an expression...
         |test_e1_1 = TestCase (assertEqual "MinusCheck" (0 -1.0) (eval s1_))
         |test_e1 = TestList [ TestLabel "1" test_e1_1 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e1
         |""".stripMargin)
  }
}
