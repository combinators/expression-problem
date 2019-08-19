package org.combinators.ep.generator     /*DI:LI:AI*/

import org.combinators.ep.domain._
import abstractions._

// TODO: Fix documentation -- Jan
/**
  * Language-independent mechanism for generating test-cases code using client-side API.
  *
  * Each language has the opportunity to enhance the test cases that already handle the basic equality check.
  * The most common situation occurs when an operation has a new data type as a result, and specialized methods
  * need to be accessed to determine the validity of the operations. In these cases, the languages-specific
  * test cases can enhance the generated code, but this is handled in the ex traits, not the Mx traits.
  *
  */
abstract class LanguageIndependentTestGenerator(val langGen: LanguageIndependentGenerator) {

  type UnitTest      /** Base concept for the representation of a single test case. */

  /**
    * Actual value in a test case.
    *
    * Each basic test case has an instance over which an operation is to be performed. This method
    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
    *
    */
  /*def actual(op: Operation, inst: Inst, params: Expression*): CodeBlockWithResultingExpressions = {
    toTargetLanguage(inst).appendDependent(instExp =>
      CodeBlockWithResultingExpressions(contextDispatch(NoSource, dispatchToExpression(instExp.head, op, params: _*)))
    )
  }*/

  /**
    * Traits can override this method to add their test cases to the mix.
    *
    * Common usage is:
    *
    * {{{
    *   abstract override def testGenerator: Seq[UnitTest] = {
    *     super.testGenerator ++ testMethod(M4_tests)
    *   }
    * }}}
    *
    * @param tests   sequence of candidate test cases
    * @return        Code fragments (based on language and approach) representing unit test cases.
    */
  def testMethods(tests:Seq[TestCase]) : Seq[UnitTest]

  /**
    * Represents the sequence of total test cases.
    */
  def testGenerator : Seq[UnitTest]

  /**
    * Test cases are placed in their own stand-along files.
    *
    * @param pkg     An optional string used for package or module declaration
    * @return
    */
  def generateSuite(pkg: Option[String]): Seq[langGen.CompilationUnit]
}
