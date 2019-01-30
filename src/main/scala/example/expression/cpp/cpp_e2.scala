package example.expression.cpp    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M0, M1, M2}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e2 extends Evolution with CPPGenerator with TestGenerator with M0 with M1 with M2 {
  self:cpp_e0 with cpp_e1 =>

  import domain._

  /** For developing test cases with strings, must convert expected value into a C++ string expression. */
  abstract override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    test.expect._1 match {
      case String => continue (new CPPElement("\"" + test.expect._2.toString + "\""))
      case _ => super.expected(test, id) (continue)
    }
  }

  abstract override def typeConverter(tpe:TypeRep) :CPPType = {
    tpe match {
      case String => new CPPType("std::string")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic, op:Operation): Seq[CPPElement] = {
    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => Seq(new CPPElement(
            s"""
               |std::ostringstream ss;
               |double val = ${valueOf(expression(exp, litValue))};
               |int ival = (int) val;
               |ss << ${valueOf(expression(exp, litValue))};
               |if (val == ival) { ss << ".0"; }  // add trailing .0 for int-value doubles
               |${result(new CPPElement("ss.str()")).mkString("\n")}
             """.stripMargin))

          case Add => result(new CPPElement(s""" "(" + ${dispatch(expression(exp, base.left), op)} + "+" + ${dispatch(expression(exp, base.right), op)} + ")" """))

          case Sub => result(new CPPElement(s""" "(" + ${dispatch(expression(exp, base.left), op)} + "-" + ${dispatch(expression(exp, base.right), op)} + ")" """))

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M2_tests)
  }
}
