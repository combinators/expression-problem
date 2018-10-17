package example.expression.cpp     /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e6 extends Evolution with CPPGenerator with VisitorCPPBinaryMethod with TestGenerator with M0 with M1 with M2 with M3 with M4 with M5 with M6 {
  self:cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5 =>

  import domain._

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Equals => scala.List[domain.Operation](AsTree)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[CPPType] = None): CPPType = {
    tpe match {
      case Boolean => new CPPType("bool")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[CPPElement] = {

    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val op = domain.AsTree.name
        Seq(new CPPElement(
          s"""
             |Astree at1;
             |e->Accept(&at1);
             |Astree at2;
             |that->Accept(&at2);
             |std::cout <<"e:";
             | at1.getValue(*(const Exp*)e)->output();
             | std::cout <<"that:";
             | at2.getValue(*(const Exp*)that)->output();
             | std::cout <<"\\n";
             |
             |value_map_[e] = at1.getValue(*(const Exp*)e)->same(
             |                at2.getValue(*(const Exp*)that));
             |
           """.stripMargin))

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val lit1 = new LitInst(1.0)
    val lit2 = new LitInst(2.0)

    val s1 = new BinaryInst(Sub, lit1, lit2)
    val lit5 = new LitInst(5.0)
    val lit6 = new LitInst(6.0)
    val sbi1 = new domain.BinaryInst(Sub, lit1, lit2)
    val abi2 = new BinaryInst(Add, lit5, lit6)
    val a2 = new BinaryInst(Add, sbi1, abi2)
    val s3 = new BinaryInst(Sub, lit1, lit2)

    super.testGenerator :+ new StandAlone("test_e6",
      s"""
         |TEST_GROUP(FirstTestGroup)
         |{
         |};
         |
         |TEST(FirstTestGroup, a1)
         |{
         |   ${convert(lit1)}
         |   ${convert(lit2)}
         |   ${convert(lit5)}
         |   ${convert(lit6)}
         |   ${convert(sbi1)}
         |   ${convert(abi2)}
         |   ${convert(s1)}
         |   ${convert(a2)}
         |   ${convert(s3)}
         |
         |   ${Equals.name.capitalize} eq_a(&${vars(a2)});
         |   ${Equals.name.capitalize} eq_b(&${vars(s3)});
         |   ${vars(s1)}.Accept(&eq_a);
         |   ${vars(s1)}.Accept(&eq_b);
         |
         |   CHECK_FALSE (eq_a.getValue(${vars{a2}}));
         |   CHECK_TRUE (eq_b.getValue(${vars{s3}}));
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
