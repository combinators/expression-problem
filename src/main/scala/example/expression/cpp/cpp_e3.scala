package example.expression.cpp    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M0, M1, M2, M3}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e3 extends Evolution with CPPGenerator with TestGenerator with M0 with M1 with M2 with M3 {
  self:cpp_e0 with cpp_e1 with cpp_e2 =>

  import domain._

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[CPPElement] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Mult => result(new CPPElement(s""" "(" + ${dispatch(atts(base.left), op)} + "*" + ${dispatch(atts(base.right), op)} + ")" """))
          case Divd => result(new CPPElement(s""" "(" + ${dispatch(atts(base.left), op)} + "/" + ${dispatch(atts(base.right), op)} + ")" """))
          case Neg => result(new CPPElement(s""" "-" + ${dispatch(atts(base.inner), op)} """))

          case _ => super.logic(exp)(op)
        }
      }

      case Eval =>
        exp match {
          case Mult => result(new CPPElement(s"${dispatch(atts(base.left), op)} * ${dispatch(atts(base.right), op)}"))
          case Divd => result(new CPPElement(s"${dispatch(atts(base.left), op)} / ${dispatch(atts(base.right), op)}"))
          case Neg => result(new CPPElement(s"- ${dispatch(atts(base.inner), op)} "))

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val tests = testMethod(M3_tests)

    super.testGenerator :+ new StandAlone("test_e3",
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
