package example.expression.generator   /*DI:LI:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

trait LanguageIndependentTestGenerator {
  val domain:BaseDomain with ModelDomain

  type UnitTest      /** Base concept for the representation of a single test case. */

  /** Represents a specific unit test which could fail. */
  def testMethod(tests:Seq[domain.TestCase]):UnitTest

  /** Represents unit test cases that execute to generate performance report. */
  def performanceMethod(): Seq[UnitTest] = Seq.empty
}
