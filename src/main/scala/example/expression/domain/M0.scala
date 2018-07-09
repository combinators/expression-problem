package example.expression.domain

trait M0 extends Evolution {
  val domain:MathDomain

  // m0:model evolution.
  // -------------------
  case object Double extends domain.TypeRep
  case object Lit extends domain.Atomic("Lit", Seq(domain.Attribute(domain.attributes.value, Double)))
  case object Add extends domain.Binary("Add")

  case object Eval extends domain.Operation("eval", Some(Double))
  class LitInst(d:Double) extends domain.AtomicInst(Lit, Some(d))

  val m0 = domain.Model("m0", Seq(Lit, Add), Seq(Eval))
  override def getModel = m0
}
