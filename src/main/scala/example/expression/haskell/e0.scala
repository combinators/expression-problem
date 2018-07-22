package example.expression.haskell    /*DD:LD:AI*/

import example.expression.domain.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends AbstractGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case Double => new HaskellType("Double")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[Haskell] = {
    val atts:Map[String,Haskell] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => Seq(new Haskell(s"""${atts(litValue)}"""))
          case Add => Seq(new Haskell(s"""${dispatch(op, atts(base.left))} + ${dispatch(op, atts(base.right))}"""))
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(5.0)

    // recursively converts (and expands) definition to be supported by functional languages
    // which offer challenges to "one-line expressions".
    val exp_a1:String = postConvert(convert("a1_", a1)).mkString("\n")
    val exp_lit1 = postConvert(convert("lit1_", lit1)).mkString("\n")

    super.testGenerator :+ new Haskell(
      s"""
         |$exp_a1
         |$exp_lit1
         |test_e0_1 = TestCase (assertEqual "PlusCheck" 3.0 (eval a1_))
         |test_e0_2 = TestCase (assertEqual "LitCheck" 5.0 (eval lit1_))
         |test_e0 = TestList [ TestLabel "1" test_e0_1, TestLabel "2" test_e0_2 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e0
         |""".stripMargin)
  }
}
