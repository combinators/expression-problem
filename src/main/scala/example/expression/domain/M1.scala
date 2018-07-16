package example.expression.domain  /*DD:LI:AI*/

trait M1 extends Evolution {
  self: M0 =>
  val domain:MathDomain
  import domain._

  // m1:model evolution.
  // -------------------
  case object Sub extends Binary("Sub")

  val m1 = Model("m1", Seq(Sub), Seq.empty, last = m0)
  override def getModel = m1
}
