package example.expression.cpp     /*DD:LD:AI*/

import example.expression.domain.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e0 extends CPPGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case Double => new CPPType("double")
      case Int => new CPPType("int")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[CPPElement] = {
    val atts:Map[String,CPPElement] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => Seq(new CPPElement(s"value_map_[e] = *${dispatch(atts(litValue),op)};"))
          case Add => Seq(new CPPElement(s"value_map_[e] = value_map_[${dispatch(atts(base.left),op)}] + value_map_[${dispatch(atts(base.right),op)}];"))
          case _ => super.logic(exp)(op)
        }

      // all future EXP sub-types can simply return hashcode.
      case Identifier => Seq(new CPPElement(s"""value_map_[e] = ${exp.hashCode()};"""))

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val lit1 = new LitInst(1.0)
    val lit2 = new LitInst(2.0)
    val a1   = new BinaryInst(Add, lit1, lit2)


    super.testGenerator :+ new StandAlone("test_e0",
      s"""
         |TEST_GROUP(FirstTestGroup)
         |{
         |};
         |
         |TEST(FirstTestGroup, a1)
         |{
         |   ${convert(lit1)}
         |   ${convert(lit2)}
         |   ${convert(a1)}
         |
         |   ${Eval.name.capitalize} e;
         |   ${vars(a1)}.Accept(&e);
         |   DOUBLES_EQUAL(3.0, e.getValue(${vars(a1)}), 0.0);
         |}
         |
         |int main(int ac, char** av)
         |{
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )
  }
}
