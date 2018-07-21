package example.expression.haskell

/*DD:LD:AI*/

import example.expression.domain._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with AbstractGenerator with TestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>
  val domain:MathDomain

   abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Haskell] = {
    val subs = subExpressions(exp)
    
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => Seq(Haskell(s""" "-" ++ a1 """))
          case Mult => Seq(Haskell(s""" "(" ++ a1 ++ "*" ++ a2 ++ ")" """))
          case Divd => Seq(Haskell(s""" "(" ++ a1 ++ "/" ++ a2 ++ ")" """))
          case _ => super.logic(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => Seq(new Haskell(s"(- a1)"))
          case Mult => Seq(new Haskell(s"a1 * a2"))
          case Divd => Seq(new Haskell(s"a1 / a2"))
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {

    // (5/7) / (7-(2*3) --> just (5/7)
    val n1 = new domain.UnaryInst(Neg, new LitInst(5.0))
    val m1 = new domain.BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val s1 = new domain.UnaryInst(Neg, m1)
    val m2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(4.0))

    val exp_n1:String = expand("n1_", n1).map(line => s"$line :: GeneralExpr").mkString("\n")
    val exp_m1:String = expand("m1_", m1).map(line => s"$line :: GeneralExpr").mkString("\n")
    val exp_m2:String = expand("m2_", m2).map(line => s"$line :: GeneralExpr").mkString("\n")

    super.testGenerator :+ new Haskell(
      s"""
         |$exp_n1
         |$exp_m1
         |$exp_m2
         |test_e3_1 = TestCase (assertEqual "NegCheck-Eval" (0-5.0) (Eval.eval n1_))
         |test_e3_2 = TestCase (assertEqual "NegCheck-Print" "-5.0" (Print.print n1_))
         |test_e3_3 = TestCase (assertEqual "MultCheck-Eval" 6.0 (Eval.eval m1_))
         |test_e3_4 = TestCase (assertEqual "MultCheck-Print" "(2.0*3.0)" (Print.print m1_))
         |test_e3_5 = TestCase (assertEqual "MultCheck-Eval" 10.0 (Eval.eval m2_))
         |test_e3_6 = TestCase (assertEqual "MultCheck-Print" "((5.0/2.0)*4.0)" (Print.print m2_))
         |
         |test_e3 = TestList [ TestLabel "1" test_e3_1, TestLabel "2" test_e3_2, TestLabel "3" test_e3_3, TestLabel "4" test_e3_4, TestLabel "5" test_e3_5, TestLabel "6" test_e3_6 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e3
         |""".stripMargin)
  }
}

