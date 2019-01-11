package example.expression.cpp    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M0, M1, M2, M3, M4}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e4 extends Evolution with CPPGenerator with TestGenerator with M0 with M1 with M2 with M3 with M4 {
  self:cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 =>

  import domain._

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Simplify => scala.List[domain.Operation](PrettyP, Eval)
      case _ => super.dependency(op)
    }
  }

  /**
    * List can be accommodated (in C++) by populating vectors with values drawn from test case.
    *
    * Calls 'continue' with an expression (the result of the prior new statements) and just concatenates all statements
    */
  override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    test.expect._1 match {
      case list:List =>
        val seq: Seq[Any] = test.expect._2.asInstanceOf[Seq[Any]]
        val ctype:CPPType = typeConverter(list)
        val inner:CPPType = typeConverter(list.generic)

        val map = seq.map(elt => s"result$id.push_back($elt);")
        val str = s"""
                     |std::vector < $inner > $ctype result$id;
                     |${map.mkString("\n")}
                     |${continue(new CPPElement(s"result$id")).mkString("\n")}
             """.stripMargin
        str.split("\n").map(line => new CPPElement(line))

      case _ => super.expected(test,id)(continue)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : CPPType = {
    tpe match {
      case el:List =>
        val tpe = typeConverter(el.generic)
        new CPPType(s"std::vector<$tpe>")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic)(op:Operation): Seq[CPPElement] = {
    val atts:Map[String,CPPElement] = subExpressions(exp)

    // generate the actual body
    op match {
      case Collect =>
        exp match {
          case Lit => Seq(new CPPElement(
            s"""
            |std::vector < double > vec;
            |            |vec.push_back(*e->getValue());
            |value_map_[e] = vec;
            |""".stripMargin))

          case Neg => Seq(new CPPElement(
            s"""
               |std::vector<double> vec;
               |std::vector<double> expv = value_map_[e->getInner()];   // HACK: FIX hard-coded attribute
               |vec.insert(vec.end(), expv.begin(), expv.end());
               |value_map_[e] = vec;
             """.stripMargin
          ))
          case Add|Sub|Mult|Divd =>
            val combined:String =
              s"""std::vector<double> vec;
                 |std::vector<double> leftv = value_map_[${dispatch(atts(base.left),op)}];
                 |std::vector<double> rightv = value_map_[${dispatch(atts(base.right),op)}];
                 |
                 |vec.insert(vec.end(), leftv.begin(), leftv.end());
                 |vec.insert(vec.end(), rightv.begin(), rightv.end());
                 |value_map_[e] = vec;
            """.stripMargin
            Seq(new CPPElement(combined))

          case _ => super.logic(exp)(op)
        }

      case Simplify =>

        exp match {
          case Lit => Seq(new CPPElement(s"""value_map_[e] = (Exp *) e;"""))

          case Add => Seq(new CPPElement(s"""
                                            |Eval eval;
                                            |e->getLeft()->Accept(&eval);
                                            |double leftV = eval.getValue(*(e->getLeft()));
                                            |e->getRight()->Accept(&eval);
                                            |double rightV = eval.getValue(*(e->getRight()));
                                            |
                                            |if (leftV + rightV == 0) {
                                            |  double z = 0;
                                            |  value_map_[e] = new Lit(&z);
                                            |} else if (leftV == 0) {
                                            |  e->getRight()->Accept(this);
                                            |  value_map_[e] = value_map_[e->getRight()];
                                            |} else if (rightV == 0) {
                                            |  e->getLeft()->Accept(this);
                                            |  value_map_[e] = value_map_[e->getLeft()];
                                            |} else {
                                            |  e->getLeft()->Accept(this);
                                            |  e->getRight()->Accept(this);
                                            |  value_map_[e] = new Add(value_map_[e->getLeft()], value_map_[e->getRight()]);
                                            |}""".stripMargin))
          case Sub => Seq(new CPPElement(s"""
                                            |Eval eval;
                                            |e->getLeft()->Accept(&eval);
                                            |double leftV = eval.getValue(*(e->getLeft()));
                                            |e->getRight()->Accept(&eval);
                                            |double rightV = eval.getValue(*(e->getRight()));
                                            |
                                            |if (leftV == rightV) {
                                            |  double z = 0;
                                            |  value_map_[e] = new Lit(&z);
                                            |} else {
                                            |  e->getLeft()->Accept(this);
                                            |  e->getRight()->Accept(this);
                                            |  value_map_[e] = new Sub(value_map_[e->getLeft()], value_map_[e->getRight()]);
                                            |}""".stripMargin))
          case Mult => Seq(new CPPElement(s"""value_map_[e] = (Exp *) e; // NOT YET IMPLEMENTED MULT """))
          case Divd => Seq(new CPPElement(s"""value_map_[e] = (Exp *) e; // NOT YET IMPLEMENTED DIVD """))

          // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
          // of inspection that might not be generalizable...
          case Neg => Seq(new CPPElement(s"""
                                            |Eval eval;
                                            |e->Accept(&eval);
                                            |if (eval.getValue(*e) == 0) {
                                            |  double z = 0;
                                            |  value_map_[e] = new Lit(&z);
                                            |} else {
                                            |  e->getInner()->Accept(this);
                                            |  value_map_[e] = new Neg(value_map_[e->getInner()]);
                                            |}""".stripMargin))
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[StandAlone] = {
    val lit1 = new LitInst(1.0)
    val lit2 = new LitInst(2.0)
    val s1   = new domain.BinaryInst(Sub, lit1, lit2)

    val tests = testMethod(M4_tests)

    super.testGenerator :+ new StandAlone("test_e4",
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
         |
         |   ${tests.mkString("\n")}
         |}
         |
         |int main(int ac, char** av)
         |{
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )
  }
}
