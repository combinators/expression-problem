package example.expression.domain  /*DD:LI:AI*/

trait M3 extends Evolution {
  self: M0 with M1 with M2 =>
  val domain:MathDomain

  // m3:model evolution.
  // -------------------
  case object Mult extends domain.Binary("Mult")
  case object Neg extends domain.Unary("Neg")
  case object Divd extends domain.Binary("Divd")

  val m3 = domain.Model("m3", Seq(Neg, Mult, Divd), Seq.empty, last = m2)
  override def getModel = m3
}
