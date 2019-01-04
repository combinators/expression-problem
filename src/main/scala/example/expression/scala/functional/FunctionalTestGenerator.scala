package example.expression.scala.functional   /*DI:LD:AD*/

import example.expression.scala.{Scala, TestGenerator}

import scala.meta.Term

trait FunctionalTestGenerator extends TestGenerator {

  /** Actual value in a test case. */
  override def actual(test:domain.TestCase, terms:Term*):Expression = {
    // had first s"new ${test.op.name.capitalize}()." +
    Scala (s"${test.op.name.toLowerCase()}(${convert(test.inst)})").term()
  }
}
