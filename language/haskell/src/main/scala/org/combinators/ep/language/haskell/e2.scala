package org.combinators.ep.language.haskell        /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M0, M1, M2, MathDomain}

/**
  * Truly independent of the specific design solution.
  */
trait e2 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain
  import domain._

  /**
    * List can be accommodated (in Haskell) as a [a,b,c,d,e]
    */
  override def expected(test:domain.TestCaseExpectedValue, id:Int) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    test.expect.tpe match {
      case String => continue (new Haskell("\"" + test.expect.inst.toString + "\""))
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

  abstract override def logic(exp:DataType, op:Operation): Seq[Haskell] = {
    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Haskell(s"(show ${expression(exp,litValue)})"))
          case Add => result(Haskell(s""""(" ++ ${dispatch(expression(exp,base.left), op)} ++ "+" ++ ${dispatch(expression(exp,base.right), op)} ++ ")""""))
          case Sub => result(Haskell(s""""(" ++ ${dispatch(expression(exp,base.left), op)} ++ "-" ++ ${dispatch(expression(exp,base.right), op)} ++ ")""""))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M2_tests)
  }
}
