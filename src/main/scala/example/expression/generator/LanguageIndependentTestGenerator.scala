package example.expression.generator    /*DI:LI:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  *
  * Each language has the opportunity to enhance the test cases that already handle the basic equality check.
  * The most common situation occurs when an operation has a new data type as a result, and specialized methods
  * need to be accessed to determine the validity of the operations. In these cases, the languages-specific
  * test cases can enhance the generated code, but this is handled in the ex traits, not the Mx traits.
  *
  * This capability is not yet fully realized.
  */
trait LanguageIndependentTestGenerator {
  val domain:BaseDomain with ModelDomain

  type UnitTest      /** Base concept for the representation of a single test case. */

  /** Represents a specific unit test which could fail. */
  def testMethod(tests:Seq[domain.TestCase]):UnitTest

  /** Represents unit test cases that execute to generate performance report. */
  def performanceMethod(): Seq[UnitTest] = Seq.empty
}
