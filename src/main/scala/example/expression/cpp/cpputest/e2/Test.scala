package example.expression.cpp.cpputest.e2

import example.expression.cpp.{CPPMethod, HasCPPCodeGenerator, HasCPPTestCaseGenerator}
import expression.Operation
import expression.data._
import expression.extensions.{PrettyP, Sub}
import expression.instances.UnitTest
import shared.compilation.CodeGeneratorRegistry

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
      s"""|  String result$num = (String) ${identifier.toString}.accept(new PrettyP());
          |  assertEquals("${tc.expected.toString}", result$num);
          |""".stripMargin.split("\n")
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }
}
