package example.expression.scala.functional   /*DI:LD:AD*/

import example.expression.scala.{Scala, TestGenerator}

import scala.meta.Term

trait FunctionalTestGenerator extends TestGenerator {

  /** Actual value in a test case. */
  override def actual(op:domain.Operation, inst:domain.AtomicInst, terms:Term*):Expression = {
    // had first s"new ${test.op.name.capitalize}()." +
    Scala (s"${op.name.toLowerCase()}(${convert(inst)})").term
  }
}
