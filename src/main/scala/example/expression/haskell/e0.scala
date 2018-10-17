package example.expression.haskell    /*DD:LD:AI*/

import example.expression.domain.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends HaskellGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case Double => new HaskellType("Double")
      case Int => new HaskellType("Int")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case Int => new Haskell("0")
      case Double => new Haskell("0.0")
      case _ => super.standardDefault(tpe)
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
          case Add => Seq(new Haskell(s"""${dispatch(atts(base.left), op)} + ${dispatch(atts(base.right), op)}"""))
          case _ => super.logic(exp)(op)
        }

      // all future EXP sub-types can simply return hashcode.
      case Identifier => Seq(new Haskell(s"${exp.hashCode()}"))
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(5.0)

    super.testGenerator :+ new Haskell(
      s"""
         |a1 =${convert(a1)}
         |lit1 = ${convert(lit1)}
         |test_e0_1 = TestCase (assertEqual "PlusCheck" 3.0 (${Eval.name} a1))
         |test_e0_2 = TestCase (assertEqual "LitCheck" 5.0 (${Eval.name} lit1))
         |test_e0 = TestList [ TestLabel "1" test_e0_1, TestLabel "2" test_e0_2 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e0
         |""".stripMargin)
  }
}
