package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait M7 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 with M5 with M6 =>
  val domain:MathDomain
  import domain._

  // m7:model evolution.
  // -------------------
  object m7_extensions {
    val target = "target"
  }

  // SquareRoot of inner value, and an operation Find that counts the number
  case object Sqrt extends Unary("Sqrt")
  case object Find extends domain.Operation("Find", Some(Int),
    Seq(domain.Parameter(m7_extensions.target, Double)))

  val m7 = Model("m7", Seq(Sqrt), Seq(Find), last = m6)

  override def getModel = m7

  val m7_1 = new UnaryInst(Sqrt, LitInst(25.0))
  val m7_2 = new BinaryInst(Add, LitInst(1.0), LitInst(2.0))
  val m7_3 = new BinaryInst(Add, LitInst(1.0), LitInst(2.0))
  val m7_4 = new BinaryInst(Add, m7_2, m7_3)

  val m7_5 = new BinaryInst(Add, LitInst(99.0), LitInst(2.0))
  val m7_6 = new BinaryInst(Add, LitInst(99.0), LitInst(2.0))
  val m7_7 = new BinaryInst(Add, m7_5, m7_6)

  def M7_tests:Seq[TestCase] = Seq(
    EqualsTestCase(m7_1, Eval, ExistsInstance(Double)(5.0)),
    EqualsTestCase(m7_4, Find, ExistsInstance(Int)(2), ExistsInstance(Double)(1.0)),
  )
}
