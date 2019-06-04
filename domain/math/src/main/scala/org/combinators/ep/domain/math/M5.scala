package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait M5 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 =>
  val domain:MathDomain
  import domain._

  // m5:model evolution.
  // -------------------
  // Represent structure as a tree
  case object Identifier extends Operation("id", Some(Int))

  val m5 = Model("m5", Seq.empty, Seq(domain.AsTree, Identifier), last = m4)
  override def getModel = m5

  // Tests
  val m5_s1 = new BinaryInst(Sub, LitInst(1.0), LitInst(976.0))
  val m5_s2 = new BinaryInst(Add, LitInst(1.0), LitInst(976.0))
  val m5_s3 = new BinaryInst(Sub, LitInst(1.0), LitInst(976.0))

  val m5_all = new BinaryInst(Sub,
    new UnaryInst(Neg, LitInst(2.0)),
    new BinaryInst(Mult,
      new BinaryInst(Sub, LitInst(1.0), LitInst(976.0)),
      new BinaryInst(Add,
        new BinaryInst(Mult, LitInst(1.0), LitInst(976.0)),
        new BinaryInst(Divd,  LitInst(1.0), LitInst(3.0)))))

  /**
    * Special test case for same queries.
    *
    * Validates that calling AsTree on inst1 yields the tree called from AsTree on inst2
    */
  case class SameTestCase(inst1:domain.Inst, inst2:domain.Inst, result:Boolean)
    extends domain.TestCase

  def M5_tests:Seq[TestCase] = Seq(
    SameTestCase(m5_s1, m5_s2, result=false),
    SameTestCase(m5_s1, m5_s3, result=true),
    SameTestCase(m5_all, m5_all, result=true)
  )
}
