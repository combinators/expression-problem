package example.expression.haskell       /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain
  import domain._

  /**
    * List can be accommodated (in Haskell) as a [a,b,c,d,e]
    */
  override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    test.expect._1 match {
      case String => continue (new Haskell("\"" + test.expect._2.toString + "\""))
      case _ => super.expected(test, id) (continue)
    }
  }

  abstract override def typeConverter(tpe:TypeRep) : HaskellType = {
    tpe match {
      case String => new HaskellType("String")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case String => new Haskell("\"\"")
      case _ => super.standardDefault(tpe)
    }
  }

  abstract override def logic(exp:Atomic)(op:Operation): Seq[Haskell] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => Seq(Haskell(s"""(show ${atts(litValue)})"""))
          case Add => Seq(Haskell(s""""(" ++ ${dispatch(atts(base.left), op)} ++ "+" ++ ${dispatch(atts(base.right), op)} ++ ")""""))
          case Sub => Seq(Haskell(s""""(" ++ ${dispatch(atts(base.left), op)} ++ "-" ++ ${dispatch(atts(base.right), op)} ++ ")""""))
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M2_tests)
  }
}
