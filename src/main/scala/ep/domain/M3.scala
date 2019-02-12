package ep.domain  /*DD:LI:AI*/

trait M3 extends Evolution {
  self: M0 with M1 with M2 =>
  val domain:MathDomain
  import domain._

  // m3:model evolution.
  // -------------------
  case object Mult extends Binary("Mult")
  case object Neg extends Unary("Neg")
  case object Divd extends Binary("Divd")

  val m3 = Model("m3", Seq(Neg, Mult, Divd), Seq.empty, last = m2)
  override def getModel = m3

  // Tests
  val m3_d1 = new domain.UnaryInst(Neg, new LitInst(5.0))
  val m3_s1 = new domain.UnaryInst(Neg, new domain.BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0)))

  val m3_m1 = new domain.UnaryInst(Neg, new LitInst(1.0))
  val m3_m2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(4.0))

  def M3_tests:Seq[TestCase] = Seq(
    EqualsTestCase(m3_m1, Eval, ExistsInstance(Double)(-1.0)),
    EqualsTestCase(m3_m1, PrettyP, ExistsInstance(String)("-1.0")),

    EqualsTestCase(m3_m2, PrettyP, ExistsInstance(String)("((5.0/2.0)*4.0)")),
    EqualsTestCase(m3_m2, Eval, ExistsInstance(Double)(10.0)),

    EqualsTestCase(m3_d1, Eval, ExistsInstance(Double)(-5.0)),
    EqualsTestCase(m3_s1, PrettyP, ExistsInstance(String)("-(2.0*3.0)"))
  )
}
