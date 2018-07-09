package example.expression.domain

trait M5 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4i with M4 =>
  val domain:MathDomain

  // m5:model evolution.
  // -------------------
  case object Boolean extends domain.TypeRep
  case object Equal extends domain.BinaryMethod("equals", Some(Boolean))
  val m5 = domain.Model("m5", Seq.empty, Seq(Equal), last = m4i)

  override def getModel = m5

}
