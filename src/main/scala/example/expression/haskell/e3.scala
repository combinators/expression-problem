package example.expression.haskell      /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>
  val domain:MathDomain
  import domain._

   abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Haskell] = {
    val atts = subExpressions(exp)
    
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => Seq(Haskell(s""" "-" ++ ${dispatch(atts(base.inner), op)} """))
          case Mult => Seq(Haskell(s""" "(" ++ ${dispatch(atts(base.left), op)} ++ "*" ++ ${dispatch(atts(base.right), op)} ++ ")" """))
          case Divd => Seq(Haskell(s""" "(" ++ ${dispatch(atts(base.left), op)} ++ "/" ++ ${dispatch(atts(base.right), op)} ++ ")" """))
          case _ => super.logic(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => Seq(new Haskell(s"(- ${dispatch(atts(base.inner), op)})"))
          case Mult => Seq(new Haskell(s"""${dispatch(atts(base.left), op)} * ${dispatch(atts(base.right), op)}"""))
          case Divd => Seq(new Haskell(s"""${dispatch(atts(base.left), op)} / ${dispatch(atts(base.right), op)}"""))
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {

    super.testGenerator :+ hunitMethod(m3, M3_tests)
//    // (5/7) / (7-(2*3) --> just (5/7)
//    val n1 = new domain.UnaryInst(Neg, new LitInst(5.0))
//    val m1 = new domain.BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
//    val s1 = new domain.UnaryInst(Neg, m1)
//    val m2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(4.0))
//
//    super.testGenerator :+ new Haskell(
//      s"""
//         |n1 = ${convert(n1)}
//         |m1 = ${convert(m1)}
//         |m2 = ${convert(m2)}
//         |test_e3_1 = TestCase (assertEqual "NegCheck-Eval" (0-5.0) (${Eval.name} n1))
//         |test_e3_2 = TestCase (assertEqual "NegCheck-Print" "-5.0" (${PrettyP.name} n1))
//         |test_e3_3 = TestCase (assertEqual "MultCheck-Eval" 6.0 (${Eval.name} m1))
//         |test_e3_4 = TestCase (assertEqual "MultCheck-Print" "(2.0*3.0)" (${PrettyP.name} m1))
//         |test_e3_5 = TestCase (assertEqual "MultCheck-Eval" 10.0 (${Eval.name} m2))
//         |test_e3_6 = TestCase (assertEqual "MultCheck-Print" "((5.0/2.0)*4.0)" (${PrettyP.name} m2))
//         |
//         |test_e3 = TestList [ TestLabel "1" test_e3_1, TestLabel "2" test_e3_2, TestLabel "3" test_e3_3, TestLabel "4" test_e3_4, TestLabel "5" test_e3_5, TestLabel "6" test_e3_6 ]
//         |
//         |main :: IO Counts
//         |main  = runTestTT test_e3
//         |""".stripMargin)
  }
}

