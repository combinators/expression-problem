package ep.scala.functional   /*DI:LD:AD*/

import ep.scala.{Scala, TestGenerator}

import scala.meta.Term

trait FunctionalTestGenerator extends TestGenerator {

  /** Actual value in a test case. */
  override def actual(op:domain.Operation, inst:domain.AtomicInst, terms:Term*):Expression = {
    Scala (s"${op.instance}(${convert(inst)})").term
  }
}
