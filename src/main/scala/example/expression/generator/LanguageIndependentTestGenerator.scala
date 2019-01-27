package example.expression.generator    /*DI:LI:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Language-independent mechanism for generating test-cases code using client-side API.
  *
  * Each language has the opportunity to enhance the test cases that already handle the basic equality check.
  * The most common situation occurs when an operation has a new data type as a result, and specialized methods
  * need to be accessed to determine the validity of the operations. In these cases, the languages-specific
  * test cases can enhance the generated code, but this is handled in the ex traits, not the Mx traits.
  *
  */
trait LanguageIndependentTestGenerator {
  val domain:BaseDomain with ModelDomain

  type UnitTest      /** Base concept for the representation of a single test case. */

  /**
    * Represents a specific unit test which could fail.
    *
    * @param tests   sequence of candidate test cases
    * @return Code fragments (based on language and approach) representing unit test cases.
    */
  def testMethod(tests:Seq[domain.TestCase]) : UnitTest

  /**
    * Represents unit test cases that execute to generate performance report.
    *
    * These tests are distinctly different from unit tests, since they are designed to time
    * the result of either the code generation or the performance of the executable code that
    * results.
    *
    * These are optional; override this method to provide your own stress-tests performance code.
    */
  def performanceMethod(): Seq[UnitTest] = Seq.empty
}