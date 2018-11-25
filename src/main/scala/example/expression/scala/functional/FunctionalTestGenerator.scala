package example.expression.scala.functional

import example.expression.scala.{Scala, TestGenerator}

trait FunctionalTestGenerator extends TestGenerator {

  /** Actual value in a test case. */
  override def actual(test:domain.TestCase):Expression = {
    // had first s"new ${test.op.name.capitalize}()." +
    Scala (dispatch(convert(test.inst), test.op).toString).expression()
  }


}
