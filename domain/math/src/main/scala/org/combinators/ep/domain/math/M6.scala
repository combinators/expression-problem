package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait M6 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 with M5 =>
  val domain:MathDomain
  import domain._

  // m5:model evolution.
  // -------------------
  case object Boolean extends TypeRep

  // Binary Methods, by definition, require special handling. Some generators
  // can short-circuit this logic, but in the most general case, it stands to reason
  // that we need to have a way to instantiate a structure that matches the expression
  // and then use those structure(s) to determine equality.
  case object Equals extends BinaryMethod("equals", Some(Boolean))
  val m6 = Model("m6", Seq.empty, Seq(Equals), last = m5)

  override def getModel = m6

  /**
    * Special test case for binary methods.
    *
    * Validates that calling AsTree on inst1 yields the tree called from AsTree on inst2
    */
  case class EqualsBinaryMethodTestCase(inst1:domain.Inst, inst2:domain.Inst, result:Boolean)
    extends domain.TestCase

  val m6_s1 = new BinaryInst(Sub, LitInst(1.0), LitInst(73.0))
  val m6_s2 = new BinaryInst(Sub, LitInst(1.0), LitInst(73.0))
  val m6_s3 = new BinaryInst(Add, LitInst(5.0), LitInst(3.0))

  val m6_m1 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, LitInst(5.0),  LitInst(2.0)), LitInst(4.0))
  val m6_m2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, LitInst(5.0),  LitInst(2.0)), LitInst(3.0))
  val m6_m3 = new domain.UnaryInst(Neg, m6_m1)

  val m6_d3 = new BinaryInst(Divd, LitInst(6.0), LitInst(2.0))
  val m6_d4 = new BinaryInst(Divd, LitInst(8.0), LitInst(2.0))

  def M6_tests:Seq[TestCase] = Seq(
    EqualsBinaryMethodTestCase(m6_s2, m6_s1, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m2, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m1, result=true),  // parameter to operation

    EqualsBinaryMethodTestCase(m6_m3, m6_m3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m3, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_d3, m6_d4, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_d3, m6_d3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_s3, m6_s3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_s3, m6_m2, result=false),  // parameter to operation

    PerformanceTestCase(
      11,
      8,
      Equals,
      new BinaryInst(Add, LitInst(1.0), LitInst(2.0)),
      Seq(ExistsInstance(baseTypeRep)(new BinaryInst(Add, LitInst(1.0), LitInst(2.0)))),
      params => params.map(param =>
        param.inst match {
          case i: Inst =>
            ExistsInstance(baseTypeRep)(new BinaryInst(Add, i, i))
          case _ => param
        }),
      inst => new BinaryInst(Add, inst, inst)
    )
  )
}
