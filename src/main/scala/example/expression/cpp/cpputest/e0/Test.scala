package example.expression.cpp.cpputest.e0

import example.expression.cpp.{CPPMethod, HasCPPCodeGenerator, HasCPPTestCaseGenerator}
import expression.Operation
import expression.data.{Add, Eval, Lit}
import expression.instances.UnitTest
import shared.compilation.CodeGeneratorRegistry

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

      s"""|  Double result$num = (Double) ${identifier.toString}.accept(new Eval());
          |  assertEquals(${tc.expected.toString}, result$num.doubleValue());
          """.stripMargin.split("\n")
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }
}
