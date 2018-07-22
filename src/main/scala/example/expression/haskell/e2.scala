package example.expression.haskell       /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with AbstractGenerator with TestGenerator with M0 with M1 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain
  import domain._

  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case String => new HaskellType("String")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:Atomic)(op:Operation): Seq[Haskell] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
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

    val exp_s1:String =  convert("s1_", s1).mkString("\n")
    val exp_a1:String =  convert("a1_", a1).mkString("\n")
    val exp_lit1 = convert("lit1_", new LitInst(5.0)).map(line => s"$line :: GeneralExpr").mkString("\n")

    super.testGenerator :+ new Haskell(
      s"""
         |$exp_s1
         |$exp_a1
         |$exp_lit1
         |test_e2_1 = TestCase (assertEqual "MinusCheck" "(1.0-2.0)" (${PrettyP.name} s1_))
         |test_e2_2 = TestCase (assertEqual "LitCheck" "5.0" (${PrettyP.name} lit1_))
         |test_e2_3 = TestCase (assertEqual "PlusCheck" "(1.0+2.0)" (${PrettyP.name} a1_))
         |
         |test_e2 = TestList [ TestLabel "1" test_e2_1, TestLabel "2" test_e2_2, TestLabel "3" test_e2_3 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e2
         |""".stripMargin)
  }
}
