package example.expression.domain  /*DD:LI:AI*/

trait M2 extends Evolution {
  self: M0 with M1 =>
  val domain:MathDomain

  // m2:model evolution.
  // -------------------
  case object String extends domain.TypeRep
  case object PrettyP extends domain.Operation("print", Some(String))

  val m2 = domain.Model("m2", Seq.empty, Seq(PrettyP), last = m1)
  override def getModel = m2
}
