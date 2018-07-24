package example.expression.haskell

/*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with AbstractGenerator with TestGenerator with M0 with M1 with M2 with M3 with M4 with M4i with M5 {
  self:e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain
  import domain._

  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case Boolean => new HaskellType("Bool")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:Atomic)(op:Operation): Seq[Haskell] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case Equal =>
        exp match {
          case Lit => Seq(Haskell(s""" show ${atts(litValue)}"""))
          case Add => Seq(Haskell(s""" "(" ++ ${dispatch(op, atts(base.left))} ++ "+" ++ ${dispatch(op, atts(base.right))} ++ ")" """))
          case Sub => Seq(Haskell(s""" "(" ++ ${dispatch(op, atts(base.left))} ++ "-" ++ ${dispatch(op, atts(base.right))} ++ ")" """))
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    val s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(5.0)

    super.testGenerator :+ new Haskell(
      s"""
         |s1 = ${convert(s1)}
         |a1 = ${convert(a1)}
         |lit1 = ${convert(lit1)}
         |test_e2_1 = TestCase (assertEqual "MinusCheck" "(1.0-2.0)" (${PrettyP.name} s1))
         |test_e2_2 = TestCase (assertEqual "LitCheck" "5.0" (${PrettyP.name} lit1))
         |test_e2_3 = TestCase (assertEqual "PlusCheck" "(1.0+2.0)" (${PrettyP.name} a1))
         |
         |test_e2 = TestList [ TestLabel "1" test_e2_1, TestLabel "2" test_e2_2, TestLabel "3" test_e2_3 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e2
         |""".stripMargin)
  }
}
