package example.expression.cpp    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M0, M1, M2, M3, M4, M5}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e5 extends Evolution with CPPGenerator with TestGenerator with M0 with M1 with M2 with M3 with M4 with M5 {
  self:cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 =>

  import domain._

  abstract override def typeConverter(tpe:domain.TypeRep) : CPPType = {
    tpe match {
      case domain.Tree => new CPPType(s"Tree *")      // internal interface (make pointer)
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[CPPElement] = {
    val atts:Map[String,CPPElement] = subExpressions(exp)

    // generate the actual body
    op match {
      case domain.AsTree => {
        val atts = subExpressions(exp)

        exp match {   // was $litValue
          case Lit =>
            val attParams = atts.map(att => "e->get" + att._2.toString.capitalize + "()").mkString(",")
            Seq(new CPPElement(s"""value_map_[e] =  new Leaf((void*) $attParams); """))

          case Add|Sub|Mult|Divd|Neg =>
            val attParams = atts.map(att => "e->get" + att._2.toString.capitalize + "()->astree()").mkString(",")
            val vec1 = new CPPElement(s"std::vector<Tree *> vec_${exp.name}{$attParams};")
            Seq(vec1, new CPPElement(s""" value_map_[e] = new Node(vec_${exp.name}, DefinedSubtypes::${exp.name.capitalize}Subtype); """))
        }
      }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val lit1 = new LitInst(1.0)
    val lit2 = new LitInst(2.0)
    val s1   = new domain.BinaryInst(Sub, lit1, lit2)
    val lit3 = new LitInst(3.0)
    val lit4 = new LitInst(4.0)
    val s2   = new domain.BinaryInst(Sub, lit3, lit4)
    val s3   = new domain.BinaryInst(Sub, lit1, lit2)


    super.testGenerator :+ new StandAlone("test_e5",
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
         |   ${convert(s1)}
         |   ${convert(s2)}
         |   ${convert(s3)}
         |
         |   ${AsTree.name.capitalize} at1;
         |   ${AsTree.name.capitalize} at2;
         |   ${AsTree.name.capitalize} at3;
         |   ${vars(s1)}.Accept(&at1);
         |   ${vars(s2)}.Accept(&at2);
         |   ${vars(s3)}.Accept(&at3);
         |   CHECK_TRUE (!at1.getValue(${vars(s1)})->same(at2.getValue(${vars(s2)})));
         |   CHECK_TRUE (at1.getValue(${vars(s1)})->same(at3.getValue(${vars(s3)})));
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
