package example.expression.scala   /*DD:LD:AI*/

import example.expression.domain.M0

import scala.meta.Stat

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends ScalaGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tr:TypeRep) : Type = {
    tr match {
      case Double => Scala("Double").tpe()
      case Int => Scala("Int").tpe()
      case _ => super.typeConverter(tr)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[Statement] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => Scala(s"${atts(litValue)}").statements()
          case Add => Scala(s"${dispatch(atts(base.left),op)} + ${dispatch(atts(base.right),op)}").statements()
          case _ => super.logic(exp)(op)
        }

        // all future EXP sub-types can simply return hashcode.
      case Identifier => Scala(s"${exp.hashCode()}").statements()

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Stat] = {
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))

    super.testGenerator :+ testMethod(M0_tests)
  }
}
