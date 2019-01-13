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
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(valueOf(atts(litValue)))
          case Add => result(new CPPElement(s"${dispatch(atts(base.left), op)} + ${dispatch(atts(base.right), op)}"))

          case _ => super.logic(exp)(op)
        }

      // all future EXP sub-types can simply return hashcode.
      case Identifier => result(new CPPElement(exp.hashCode().toString))

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val tests = testMethod(M0_tests)

    super.testGenerator :+ new StandAlone("test_e0",
      s"""
         |TEST_GROUP(FirstTestGroup)
         |{
         |};
         |
         |TEST(FirstTestGroup, a1)
         |{
         |   ${tests.mkString("\n")}
         |}
         |
         |int main(int ac, char** av)
         |{
         |  MemoryLeakWarningPlugin::turnOffNewDeleteOverloads();
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )
  }
}
