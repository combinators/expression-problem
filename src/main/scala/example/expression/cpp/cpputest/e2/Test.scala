package example.expression.cpp.cpputest.e2

import example.expression.cpp.HasCPPTestCaseGenerator
import expression.Operation
import expression.extensions.PrettyP
import expression.instances.UnitTest

/**
 * Designed knowing this comes after E1, and thus must account for Lit, Add (E0) and Sub (E1)
 */
trait Test extends HasCPPTestCaseGenerator {

  /**
    * Create test case code for eval where the expression "identifier"  has already been constructed
    * and the test case is UnitTest, which has its own expectations.
    *
    * Forms chain of responsibility
    */
  abstract override def testCaseGenerator(op:Operation, identifier:String, tc: UnitTest) : Seq[String] = {

    if (op.equals(new PrettyP)) {
      val num: Int = nextTestNumber()

      s"""|  PrettyP pp;
          |  ${identifier.toString}.Accept(&pp);
          |  STRCMP_EQUAL("${tc.expected.toString}", pp.getValue(${identifier.toString}).c_str());"""
          .stripMargin.split("\n")
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }
}
