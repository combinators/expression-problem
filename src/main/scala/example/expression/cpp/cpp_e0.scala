package example.expression.cpp

/*DD:LD:AI*/

import example.expression.domain.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e0 extends AbstractGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[CPPType] = None) : CPPType = {
    tpe match {
      case Double => new CPPType("Double")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[CPPElement] = {
    val atts:Map[String,CPPElement] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => Seq(new CPPElement(s"return ${atts(litValue)};"))
          case Add => Seq(new CPPElement(s"return ${dispatch(atts(base.left),op)} + ${dispatch(atts(base.right),op)};"))
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[CPPElement] = {
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val lit1 = new LitInst(5.0)

    super.testGenerator ++ Seq(
      new CPPElement("assertEquals(3.0, ${dispatch(convert(a1), Eval)});"),
      new CPPElement("assertEquals(5.0, ${dispatch(convert(lit1), Eval)});"))
  }
}
