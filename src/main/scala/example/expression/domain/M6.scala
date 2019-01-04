package example.expression.domain   /*DD:LI:AI*/

trait M6 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 with M5 =>
  val domain:MathDomain
  import domain._

  // m5:model evolution.
  // -------------------
  case object Boolean extends TypeRep

  // Binary Methods, by definition, require special handling. Some generators
  // can short-circuit this logic, but in the most general case, it stands to reason
  // that we need to have a way to instantiate a structure that matches the expression
  // and then use those structure(s) to determine equality.
  case object Equals extends BinaryMethod("equals", Some(Boolean))
  val m6 = Model("m6", Seq.empty, Seq(Equals), last = m5)

  override def getModel = m6

  val m6_s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
  val m6_s2 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

  def M6_tests:Seq[TestCase] = Seq(
    EqualsTestCase(m6_s2, Equals, (Boolean, true), (Exp, m6_s1)),  // parameter to operation
    // qualsTestCase(new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0)), (Double, 3.0), Eval),
  )
}
