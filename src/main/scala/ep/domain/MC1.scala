package ep.domain    /*DD:LI:AI*/

trait MC1 extends Evolution {
  self:  M0 with M1 with M2 with M3 with I1 with I2 =>
  val domain:MathDomain
  import domain._

  // m3 x i2:model evolution.
  // -------------------
  override def getModel = m3.merge("c1", i2)

  val mc1_s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
  val mc1_d1 = new domain.BinaryInst(Divd, new LitInst(1.0),
    new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)))
  val mc1_s2 = new domain.UnaryInst(Inv, mc1_s1)

  // testing
  def MC1_tests: Seq[TestCase] = Seq(
    EqualsTestCase(mc1_s2, PrettyP, (String, "(1.0/(1.0-2.0))")),
    EqualsTestCase(mc1_d1, PrettyP, (String, "(1.0/(1.0-2.0))"))
  )

}
