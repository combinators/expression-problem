package shared.compilation

import expression.instances.UnitSuite

/**
  * Offers tests.
  */
trait HasTests {

  def testCases: UnitSuite
}
