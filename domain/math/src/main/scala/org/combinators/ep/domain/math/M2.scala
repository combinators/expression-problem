package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait M2 extends Evolution {
  self: M0 with M1 =>
  val domain: MathDomain

  import domain._

  // m2:model evolution.
  // -------------------
  case object String extends TypeRep {
    override type scalaInstanceType = java.lang.String
  }

  case object PrettyP extends Operation("prettyp", Some(String)) // PROBLEMS WITH "print"

  val m2 = Model("m2", Seq.empty, Seq(PrettyP), last = m1)

  override def getModel: Model = m2

  // Tests
  val m2_s1 = new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0))

  def M2_tests: Seq[TestCase] = Seq(
    EqualsTestCase(m2_s1, PrettyP, ExistsInstance(String)("(1.0-2.0)")),

    EqualsTestCase(new domain.BinaryInst(Add, m2_s1, new domain.BinaryInst(Add, LitInst(5.0), LitInst(6.0))),
      PrettyP, ExistsInstance(String)("((1.0-2.0)+(5.0+6.0))"))
  )
}
