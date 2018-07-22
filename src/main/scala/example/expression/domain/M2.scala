package example.expression.domain  /*DD:LI:AI*/

trait M2 extends Evolution {
  self: M0 with M1 =>
  val domain:MathDomain
  import domain._

  // m2:model evolution.
  // -------------------
  case object String extends TypeRep
  case object PrettyP extends Operation("prettyp", Some(String))  // PROBLEMS WITH "print"

  val m2 = Model("m2", Seq.empty, Seq(PrettyP), last = m1)
  override def getModel = m2
}
