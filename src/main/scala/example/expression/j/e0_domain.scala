package example.expression.j

import example.expression.domain.Domain

trait e0_domain {
  val domain:Domain
  import domain._

  // e0:model evolution.
  // -------------------
  object Domain {
    case object Double extends types.Types
    case object Lit extends expressions.Exp("Lit", Attribute(attributes.value, Double))
    case object Add extends expressions.BinaryExp("Add")
    case object Eval extends Operation("eval", Some(Double))
    class LitInst(d: Double) extends instances.ExpInst(Lit, Some(d))
  }
}
