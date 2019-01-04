package example.expression.domain   /*DD:LI:AI*/

trait S1 extends Evolution {
  self: S0 =>
  val domain: ShapeDomain
  import domain._

  // m1:model evolution.
  // -------------------
  case object Shrink extends domain.Operation("shrink", Some(domain.Shape), Seq((pct, Double)))

  val s1 = domain.Model("s1", Seq.empty, Seq(Shrink), s0)
  override def getModel:Model = s1

//
//  // testing
//  def M1_tests: Seq[TestCase] = Seq(
//    EqualsTestCase(new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)), Eval, (Double, -1.0))
//  )
}

