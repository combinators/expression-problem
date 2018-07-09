package example.expression.domain

trait M1 extends Evolution {
  self: M0 =>
  val domain:MathDomain

  // m1:model evolution.
  // -------------------
  case object Sub extends domain.Binary("Sub")

  val m1 = domain.Model("m1", Seq(Sub), Seq.empty, last = m0)
  override def getModel = m1
}
