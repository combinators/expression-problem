package example.expression.cpp    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M0, M1, M2, MathDomain}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e2 extends Evolution with AbstractGenerator with TestGenerator with M0 with M1 with M2 {
  self:cpp_e0 with cpp_e1 =>

  import domain._

  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[CPPType] = None) :CPPType = {
    tpe match {
      case String => new CPPType("std::string")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[CPPElement] = {
    val atts:Map[String,CPPElement] = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => Seq(new CPPElement(
            s"""
               |std::ostringstream ss;
               |double val = *e->getValue();
               |int ival = (int) val;
               |ss << *e->getValue();
               |if (val == ival) { ss << ".0"; }  // add trailing .0 for int-value doubles
               |value_map_[e] = ss.str();
               |
             """.stripMargin))
          case Add => Seq(new CPPElement(
            s""" value_map_[e] = "(" + value_map_[${dispatch(atts(base.left),op)}] + "+" + value_map_[${dispatch(atts(base.right),op)}] + ")"; """))

          case Sub => Seq(new CPPElement(
            s""" value_map_[e] = "(" + value_map_[${dispatch(atts(base.left),op)}] + "-" + value_map_[${dispatch(atts(base.right),op)}] + ")"; """))

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val lit1 = new LitInst(1.0)
    val lit2 = new LitInst(2.0)
    val s1   = new domain.BinaryInst(Sub, lit1, lit2)

    super.testGenerator :+ new StandAlone("test_e2",
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
         |   ${PrettyP.name.capitalize} pp;
         |   ${vars(s1)}.Accept(&pp);
         |   STRCMP_EQUAL("(1.0-2.0)", pp.getValue(${vars(s1)}).c_str());
         |}
         |
         |int main(int ac, char** av)
         |{
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )
  }
}
