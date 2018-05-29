package example.expression.cpp

import expression.Operation
import expression.instances.UnitTest
import org.combinators.templating.twirl.Java

/**
  * Creates a chain of responsibility for producing C++ CPPUnit statements to evaluate the correctness of
  * the implementation.
  */
trait HasCPPTestCaseGenerator {

  var _testNumber:Int = 0

  /** Advance to next test and return its number. */
  def nextTestNumber(): Int = {
    _testNumber += 1

    _testNumber
  }

  /**
    * Given a desired operation to test, a C++ identifier of the instantiated expression, and the
    * desired UnitTest, produce code that ensures the actual value equals the expected value of the test case
    *
    * @param op           Operation being evaluated
    * @param identifier   Java name of the instantiated expression
    * @param tc           Test case under consideration
    *
    * @return             Seq[Statement] containing JUnit assertions as needed
    */
  def testCaseGenerator(op:Operation, identifier:String, tc: UnitTest) : Seq[String] = {
    // skip for lack of anything better.
    s"// skip${op.name} ${identifier.toString}".split("\n")
  }

}
