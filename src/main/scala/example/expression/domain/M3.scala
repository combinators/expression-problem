package example.expression.domain  /*DD:LI:AI*/

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
    EqualsTestCase(m3_m1, (Double, -1.0), Eval),
    EqualsTestCase(m3_m1, (String, "-1.0"), PrettyP),

    EqualsTestCase(m3_m2, (String, "((5.0/2.0)*4.0)"), PrettyP),
    EqualsTestCase(m3_m2, (Double, 10.0), Eval),

    EqualsTestCase(m3_d1, (Double, -5.0), Eval),
    EqualsTestCase(m3_s1, (String, "-(2.0*3.0)"), PrettyP)
  )
}
