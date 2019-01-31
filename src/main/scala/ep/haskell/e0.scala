package ep.haskell    /*DD:LD:AI*/

import ep.domain.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends HaskellGenerator with HUnitTestGenerator with M0 {
  import domain._

  /**
    * negative numbers in haskell can't be simple -1.0 but must be (0 -1.0) for some reason?
    */
  override def expected(test:domain.TestCaseExpectedValue, id:Int) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    test.expect._1 match {
      case Double => {
        val dbl:Double = test.expect._2.asInstanceOf[Double]

        // Can't seem to use unary negation operation in Haskell ?!
        val expect = if (dbl < 0) {
          s"(0 - ${math.abs(dbl)})"
        } else {
          dbl.toString
        }
        continue (new Haskell(expect))
      }

      case _ => super.expected(test, id) (continue)
    }
  }

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep) : HaskellType = {
    tpe match {
      case Double => new HaskellType("Double")
      case Int => new HaskellType("Int")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case Int =>  Haskell("0")
      case Double => new Haskell("0.0")
      case _ => super.standardDefault(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic, op:Operation): Seq[Haskell] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(new Haskell(s"""${expression(exp,litValue)}"""))
          case Add => result(new Haskell(s"""${dispatch(expression(exp,base.left), op)} + ${dispatch(expression(exp,base.right), op)}"""))
          case _ => super.logic(exp, op)
        }

      // all future EXP sub-types can simply return hashcode.
      case Identifier => result(new Haskell(s"${exp.hashCode()}"))
      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {

    super.testGenerator :+ hunitMethod(M0_tests)
  }
}
