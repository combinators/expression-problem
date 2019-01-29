package example.expression.cpp    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M0, M1, M2, M3, M4}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e4 extends Evolution with CPPGenerator with TestGenerator with CPPProducer with M0 with M1 with M2 with M3 with M4 {
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
        //val inner:CPPType = typeConverter(list.generic)

        val map = seq.map(elt => s"result$id.push_back($elt);")
        val str = s"""
                     |$ctype result$id;
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
  abstract override def logic(exp:Atomic, op:Operation): Seq[CPPElement] = {
    val atts:Map[String,CPPElement] = subExpressions(exp)

    // generate the actual body
    val source = Source(exp,op)
    op match {
      case Collect =>
        val tpe = op.returnType.get match {
          case list:List => typeConverter(list.generic)
        }
        exp match {
          case Lit => Seq(new CPPElement(
            s"""
            |std::vector < $tpe > vec;
            |vec.push_back(${valueOf(atts(litValue))});
            |${result(new CPPElement("vec")).mkString("\n")};""".stripMargin))

          case Neg => Seq(new CPPElement(
            s"""
               |std::vector<$tpe> vec;
               |std::vector<$tpe> expv = ${dispatch(atts(base.inner),op)};
               |vec.insert(vec.end(), expv.begin(), expv.end());
               |${result(new CPPElement("vec")).mkString("\n")};""".stripMargin))
          case Add|Sub|Mult|Divd => Seq(new CPPElement(
              s"""std::vector< $tpe > vec;
                 |std::vector< $tpe > leftv = ${dispatch(atts(base.left),op)};
                 |std::vector< $tpe > rightv = ${dispatch(atts(base.right),op)};
                 |
                 |vec.insert(vec.end(), leftv.begin(), leftv.end());
                 |vec.insert(vec.end(), rightv.begin(), rightv.end());
                 |${result(new CPPElement("vec")).mkString("\n")};""".stripMargin))

          case _ => super.logic(exp, op)
        }

      case Simplify =>
        val zero = new CPPElement("0.0")
        val one = new CPPElement("1.0")
        val negOne = new CPPElement("-1.0")
        exp match {
            // STILL has work to do...
          case Lit =>
            val value = new CPPElement(s"${valueOf(atts(litValue))}")
            Seq(new CPPElement(s"""${result(inst(Lit, value)).mkString("\n")} """))


          case Add =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(new CPPElement(s"""
                      |double leftV = ${contextDispatch(source, deltaLeft)};
                      |double rightV = ${contextDispatch(source, deltaRight)};
                      |if (leftV + rightV == 0) {
                      |  ${result(inst(Lit, zero)).mkString("\n")}
                      |} else if (leftV == 0) {
                      |  ${result(dispatch(atts(domain.base.right), Simplify)).mkString("\n")}
                      |} else if (rightV == 0) {
                      |  ${result(dispatch(atts(domain.base.left), Simplify)).mkString("\n")}
                      |} else {
                      |  ${result(inst(Add, dispatch(atts(domain.base.left), Simplify),dispatch(atts(domain.base.right), Simplify))).mkString("\n")}
                      |}""".stripMargin))
          case Sub =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(new CPPElement(s"""
                      |double leftV = ${contextDispatch(source, deltaLeft)};
                      |double rightV = ${contextDispatch(source, deltaRight)};
                      |if (leftV == rightV) {
                      |  ${result(inst(Lit, zero)).mkString("\n")}
                      |} else {
                      |  ${result(inst(Sub, dispatch(atts(domain.base.left), Simplify),dispatch(atts(domain.base.right), Simplify))).mkString("\n")}
                      |}""".stripMargin))

          case Mult =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(new CPPElement(s"""
                      |double leftV = ${contextDispatch(source, deltaLeft)};
                      |double rightV = ${contextDispatch(source, deltaRight)};
                      |if (leftV == 0 || rightV == 0) {
                      |  ${result(inst(Lit, zero)).mkString("\n")}
                      |} else if (leftV == 1) {
                      |  ${result(dispatch(atts(domain.base.right), Simplify)).mkString("\n")}
                      |} else if (rightV == 1) {
                      |  ${result(dispatch(atts(domain.base.left), Simplify)).mkString("\n")}
                      |} else {
                      |  ${result(inst(Mult, dispatch(atts(domain.base.left), Simplify),dispatch(atts(domain.base.right), Simplify))).mkString("\n")}
                      |}""".stripMargin))
          case Divd =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(new CPPElement(s"""
                      |double leftV = ${contextDispatch(source, deltaLeft)};
                      |double rightV = ${contextDispatch(source, deltaRight)};
                      |if (leftV == 0) {
                      |  ${result(inst(Lit, zero)).mkString("\n")}
                      |} else if (rightV == 1) {
                      |  ${result(dispatch(atts(domain.base.left), Simplify)).mkString("\n")}
                      |} else if (leftV == rightV) {
                      |   ${result(inst(Lit, one)).mkString("\n")}
                      |} else if (leftV == -rightV) {
                      |   ${result(inst(Lit, negOne)).mkString("\n")}
                      |} else {
                      |  ${result(inst(Divd, dispatch(atts(domain.base.left), Simplify),dispatch(atts(domain.base.right), Simplify))).mkString("\n")}
                      |}""".stripMargin))

          // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
          // of inspection that might not be generalizable...
          case Neg =>
            val deltaInner = deltaChildOp(source, domain.base.inner, Eval)
            Seq(new CPPElement(s"""
                      |if (${contextDispatch(source, deltaInner)} == 0) {
                      |   ${result(inst(Lit, zero)).mkString("\n")}
                      |} else {
                      |   ${result(inst(Neg, dispatch(atts(domain.base.inner), Simplify))).mkString("\n")}
                      |}""".stripMargin))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M4_tests)
  }
}
