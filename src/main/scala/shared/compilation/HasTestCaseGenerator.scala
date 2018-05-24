package shared.compilation

import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import expression.Operation
import expression.instances.UnitTest
import org.combinators.templating.twirl.Java

/**
  * Creates a chain of responsibility for producing Java JUnit statements to evaluate the correctness of
  * the implementation.
  */
trait HasTestCaseGenerator {

  var _testNumber:Int = 0

  /** Advance to next test and return its number. */
  def nextTestNumber(): Int = {
    _testNumber += 1

    _testNumber
  }

//  /**
//    * When an expression requires operations o1, o2, then the subtypes must be instantiated from the
//    * most specific subclasses available, derived from the operations to be required of the expr
//    *
//    * Process MustHaves
//    *
//    * @return
//    */
//  def computeSubTypes() : String = {
//    if (subTypes.equals("")) {
//      var ops: List[String] = List.empty
//      for (ut: UnitTest <- expr.asScala) {
//
//        // ignore Eval, which is assumed to always be there
//        if (!ut.op.getClass.getSimpleName.equals("Eval") && !ops.contains(ut.op.getClass.getSimpleName)) {
//          ops = ops :+ ut.op.getClass.getSimpleName
//        }
//
//        mustHave.foreach (op => {
//          if (!ops.contains(op.getClass.getSimpleName)) {
//            ops = ops :+ op.getClass.getSimpleName
//          }
//        })
//      }
//
//      subTypes = ops.sortWith(_ < _).mkString("")
//    }
//
//    subTypes
//  }

  /**
    * Given a desired operation to test, a Java identifier of the instantiated expression, and the
    * desired UnitTest, produce code that ensures the actual value equals the expected value of the test case
    *
    * @param op           Operation being evaluated
    * @param identifier   Java name of the instantiated expression
    * @param tc           Test case under consideration
    *
    * @return             Seq[Statement] containing JUnit assertions as needed
    */
  def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement] = {
    // skip for lack of anything better.
    Java(s"// skip${op.name} ${identifier.toString}").statements()
  }

}
