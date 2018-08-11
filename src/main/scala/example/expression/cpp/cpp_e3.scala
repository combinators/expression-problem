package example.expression.cpp

/*DD:LD:AI*/

import example.expression.domain.{Evolution, M0, M1, M2, M3}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e3 extends Evolution with AbstractGenerator with TestGenerator with M0 with M1 with M2 with M3 {
  self:cpp_e0 with cpp_e1 with cpp_e2 =>

  import domain._

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[CPPElement] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Mult => Seq(new CPPElement(
            s""" value_map_[e] = "(" + value_map_[${dispatch(atts(base.left),op)}] + "*" + value_map_[${dispatch(atts(base.right),op)}] + ")"; """))
          case Divd => Seq(new CPPElement(
            s""" value_map_[e] = "(" + value_map_[${dispatch(atts(base.left),op)}] + "/" + value_map_[${dispatch(atts(base.right),op)}] + ")"; """))
          case Neg => Seq(new CPPElement(
            s""" value_map_[e] = "-" + value_map_[${dispatch(atts(base.inner),op)}]; """))

          case _ => super.logic(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Mult => Seq(new CPPElement(
            s""" value_map_[e] =  value_map_[${dispatch(atts(base.left),op)}] * value_map_[${dispatch(atts(base.right),op)}]; """))
          case Divd => Seq(new CPPElement(
            s""" value_map_[e] =  value_map_[${dispatch(atts(base.left),op)}] / value_map_[${dispatch(atts(base.right),op)}]; """))
          case Neg => Seq(new CPPElement(
            s""" value_map_[e] =  -value_map_[${dispatch(atts(base.inner),op)}]; """))
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val lit1 = new LitInst(1.0)
    val n1 = new domain.UnaryInst(Neg, lit1)

    // (5/7) / (7-(2*3) --> just (5/7)
    val lit5 = new LitInst(5.0)
    val d1 = new domain.UnaryInst(Neg, lit5)
    val lit2 = new LitInst(2.0)
    val lit3 = new LitInst(3.0)
    val m1 = new domain.BinaryInst(Mult, lit2, lit3)
    val n2 = new domain.UnaryInst(Neg, m1)

    val d2 = new domain.BinaryInst(Divd, lit5, lit2)
    val lit4 = new LitInst(4.0)
    val m2 = new domain.BinaryInst(Mult, d2, lit4)

    super.testGenerator :+ new StandAlone("test_e3",
      s"""
         |TEST_GROUP(FirstTestGroup)
         |{
         |};
         |
         |TEST(FirstTestGroup, a1)
         |{
         |   ${convert(lit1)}
         |   ${convert(lit2)}
         |   ${convert(lit3)}
         |   ${convert(lit4)}
         |   ${convert(lit5)}
         |   ${convert(n1)}
         |   ${convert(m1)}
         |   ${convert(n2)}
         |   ${convert(d2)}
         |   ${convert(m2)}
         |   ${convert(d1)}

         |   ${PrettyP.name.capitalize} pp;
         |   ${Eval.name.capitalize} e;
         |
         |   ${vars(n2)}.Accept(&e);
         |   DOUBLES_EQUAL(-6.0, e.getValue(${vars(n2)}), 0.0);
         |
         |   ${vars(n1)}.Accept(&pp);
         |   STRCMP_EQUAL("-1.0", pp.getValue(${vars(n1)}).c_str());
         |
         |   ${vars(n1)}.Accept(&e);
         |   DOUBLES_EQUAL(-1.0, e.getValue(${vars(n1)}), 0.0);
         |
         |   ${vars(m2)}.Accept(&pp);
         |   STRCMP_EQUAL("((5.0/2.0)*4.0)", pp.getValue(${vars(m2)}).c_str());
         |
         |   ${vars(n2)}.Accept(&pp);
         |   STRCMP_EQUAL("-(2.0*3.0)", pp.getValue(${vars(n2)}).c_str());
         |}
         |
         |int main(int ac, char** av)
         |{
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )
  }
}
