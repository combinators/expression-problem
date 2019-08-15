package org.combinators.ep.domain.math

import org.combinators.ep.domain._

class M2(val m1:M1) extends Evolution {

  val domain:BaseDomain = m1.domain
  import domain._
  import m1._
  import m0._    // note by ordering in this fashion, you will bring in m0

  // m2:model evolution.
  // -------------------
  case object String extends TypeRep {
    override type scalaInstanceType = java.lang.String
  }

  case object PrettyP extends Operation("prettyp", String) // PROBLEMS WITH "print"

  val m2 = Model("m2", Seq.empty, Seq(PrettyP), last = m1.getModel)

  override def getModel: Model = m2

  // Tests
  val m2_s1 = new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0))

  def M2_tests: Seq[TestCase] = Seq(
    EqualsTestCase(m2_s1, PrettyP, ExistsInstance(String)("(1.0-2.0)")),

    EqualsTestCase(new domain.BinaryInst(Add, m2_s1, new domain.BinaryInst(Add, LitInst(5.0), LitInst(6.0))),
      PrettyP, ExistsInstance(String)("((1.0-2.0)+(5.0+6.0))"))
  )
}
