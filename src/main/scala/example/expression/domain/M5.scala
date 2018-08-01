package example.expression.domain  /*DD:LI:AI*/

trait M5 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 =>
  val domain:MathDomain

  // m5:model evolution.
  // -------------------
  // m5: graft in a useful operation for future operations to use

  val m5 = domain.Model("m5", Seq.empty, Seq(domain.AsTree), last = m4)
  override def getModel = m5
}
