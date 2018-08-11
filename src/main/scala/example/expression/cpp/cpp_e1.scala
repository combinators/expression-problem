package example.expression.cpp   /*DD:LD:AI*/

import example.expression.domain.{Evolution, M1}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e1 extends Evolution with AbstractGenerator with TestGenerator with M1 {
  self:cpp_e0 =>

  import domain._

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[CPPElement] = {
    val atts:Map[String,CPPElement] = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Sub => Seq(new CPPElement(s"value_map_[e] = value_map_[${dispatch(atts(base.left),op)}] - value_map_[${dispatch(atts(base.right),op)}];"))
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val lit1 = new LitInst(1.0)
    val lit2 = new LitInst(2.0)
    val s1   = new BinaryInst(Sub, lit1, lit2)

    super.testGenerator :+ new StandAlone("test_e1",
      s"""
         |TEST_GROUP(FirstTestGroup)
         |{
         |};
         |
         |TEST(FirstTestGroup, a1)
         |{
         |   ${convert(lit1)}
         |   ${convert(lit2)}
         |   ${convert(s1)}
         |
         |   ${Eval.name.capitalize} e;
         |   ${vars(s1)}.Accept(&e);
         |   DOUBLES_EQUAL(-1.0, e.getValue(${vars(s1)}), 0.0);
         |}
         |
         |int main(int ac, char** av)
         |{
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )
  }
}
