package org.combinators.ep.language.gj   /*DD:LD:AI*/

import org.combinators.ep.domain.math.M0

/**
  * Truly independent of the specific design solution.
  *
  * http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt
  * Still Java-based, naturally and JUnit
  */
trait e0 extends GJGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep) : Type = {
    tpe match {
      case Double => new GJType("Double")
      case Int => new GJType("Integer")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:DataType, op:Operation): Seq[GJStatement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(GJ(expression(exp, litValue).toString))
          case Add => result(GJ(s"new Double(${dispatch(expression(exp, base.left),op)}.doubleValue() + ${dispatch(expression(exp, base.right),op)}.doubleValue())"))
          case _ => super.logic(exp, op)
        }

        // all future EXP sub-types can simply return hashcode.
      case Identifier => result(GJ(exp.hashCode().toString))

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[UnitTest] = {
    super.testGenerator ++ testMethod(M0_tests)
//    val a1 = new BinaryInst(Add, LitInst(1.0), LitInst(2.0))
//    val lit1 = LitInst(5.0)
//    val modName = getModel.name
//
//    //  TODO: change convert to toTargetLanguage
//    super.testGenerator :+ Seq(GJStatement(
//      s"""|   Lang$modName l = new Lang$modName();
//          |   assertEquals(3.0, ${testDispatch(convert(a1), Eval)});
//          |   assertEquals(5.0, ${testDispatch(convert(lit1), Eval)});
//          |""".stripMargin))
  }
}
