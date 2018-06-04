package example.expression.cpp.cpputest.e0

import example.expression.cpp.HasCPPTestCaseGenerator
import expression.Operation
import expression.data.Eval
import expression.instances.UnitTest

/**
  * Each trait is stand alone, and woven into the final repository.
  */
trait Test extends  HasCPPTestCaseGenerator {

  /**
    * Create test case code for eval where the expression "identifier"  has already been constructed
    * and the test case is UnitTest, which has its own expectations.
    *
    * Forms chain of responsibility
    */
  abstract override def testCaseGenerator(op:Operation, identifier:String, tc: UnitTest) : Seq[String] = {

    // only handle Eval
    if (op.equals(new Eval)) {
      val num: Int = nextTestNumber()

      s"""|  Eval e4;
          |  ${identifier.toString}.Accept(&e4);
          |  DOUBLES_EQUAL(${tc.expected.toString}, e4.getValue(${identifier.toString}), 0.0);
          """.stripMargin.split("\n")
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }
}
