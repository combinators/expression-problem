package example.expression.domain  /*DD:LI:AI*/

trait M6 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 with M5 =>
  val domain:MathDomain

  // m5:model evolution.
  // -------------------
  case object Boolean extends domain.TypeRep

  // Binary Methods, by definition, require special handling. Some generators
  // can short-circuit this logic, but in the most general case, it stands to reason
  // that we need to have a way to instantiate a structure that matches the expression
  // and then use those structure(s) to determine equality.
  case object Equals extends domain.BinaryMethod("equals", Some(Boolean))
  val m6 = domain.Model("m6", Seq.empty, Seq(Equals), last = m5)

  override def getModel = m6
}
