package ep.domain  /*DD:LI:AI*/

trait M4 extends Evolution  {
  self: M0 with M1 with M2 with M3 =>
  val domain:MathDomain
  import domain._

  // m4:model evolution.
  // -------------------
  case object Simplify extends ProducerOperation("simplify")
  case class List(generic:TypeRep) extends TypeRep
  case object Collect extends Operation("collect", Some(List(Double)))

  val m4 = domain.Model("m4", Seq.empty, Seq(Simplify, Collect), last = m3)
  override def getModel:Model = m4

  // Tests
  // (5/7) / (7-(2*3) --> just (5/7)
  val m4_m1 = new BinaryInst(Mult, new BinaryInst (Divd, new LitInst(5.0), new LitInst(2.0)), new LitInst(4.0))
  val m4_m2 = new BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
  val m4_d2 = new BinaryInst(Divd, new BinaryInst(Divd, new LitInst(5.0), new LitInst(7.0)), new BinaryInst(Sub, new LitInst(7.0), m4_m2))

  val m4_s_0 = new UnaryInst(Neg, new LitInst(0.0))
  val m4_s_5 = new BinaryInst(Add, new LitInst(5.0), new LitInst(0.0))
  val m4_s_00 = new BinaryInst(Add, new LitInst(0.0), new LitInst(0.0))
  val m4_s_7 = new BinaryInst(Add, new LitInst(0.0), new LitInst(7.0))
  val m4_s_13 = new BinaryInst(Mult, new LitInst(13.0), new LitInst(1.0))
  val m4_s_12 = new BinaryInst(Mult, new LitInst(1.0), new LitInst(12.0))
  val m4_s_m0 = new BinaryInst(Sub, new LitInst(7.0), new LitInst(7.0))

  val m4_s_n1 = new BinaryInst(Divd, new LitInst(5.0), new LitInst(-5.0))
  val m4_s_1 = new BinaryInst(Divd, new LitInst(-5.0), new LitInst(-5.0))
  val m4_s_d0 = new BinaryInst(Divd, new LitInst(0.0), new LitInst(-5.0))

  /**
    * Test cases for Simplify are oddly complicated. The Simplify operation returns a new Exp object, but
    * making test cases depends upon having the ability to PrettyP the result. We therefore want to check
    * equality of (d1 x prettyP) vs. ((d2 x Simplify) x prettyp)
    *
    * Result should support a composite operation
    */
  def M4_tests:Seq[TestCase] = Seq(
    EqualsTestCase(m4_d2, Collect, (List(Double), Seq(5.0, 7.0, 7.0, 2.0, 3.0))),
    EqualsTestCase(m4_s_00, Collect, (List(Double), Seq(0.0, 0.0))),
    EqualsTestCase(m4_s_0, Collect, (List(Double), Seq(0.0))),
    EqualsTestCase(m4_s_12, Collect, (List(Double), Seq(1.0, 12.0))),
    EqualsTestCase(m4_s_13, Collect, (List(Double), Seq(13.0, 1.0))),
    EqualsTestCase(m3_m1, PrettyP, (String, "-1.0")),

    EqualsTestCase(m4_m1, PrettyP, (String, "((5.0/2.0)*4.0)")),
    EqualsTestCase(m4_m1, Eval, (Double, 10.0))
  )

  def M4_simplify_tests:Seq[TestCase] = Seq(
    EqualsCompositeTestCase(m4_d2, Seq(Simplify, PrettyP), (String, "(5.0/7.0)")),
    EqualsCompositeTestCase(m4_s_0, Seq(Simplify, Eval), (Double, 0.0)),
    EqualsCompositeTestCase(m4_s_5, Seq(Simplify, Eval), (Double, 5.0)),
    EqualsCompositeTestCase(m4_s_7, Seq(Simplify, Eval), (Double, 7.0)),
    EqualsCompositeTestCase(m4_s_13 , Seq(Simplify, Eval), (Double, 13.0)),
    EqualsCompositeTestCase(m4_s_12 , Seq(Simplify, Eval), (Double, 12.0)),
    EqualsCompositeTestCase(m4_s_m0 , Seq(Simplify, Eval), (Double, 0.0)),

    EqualsCompositeTestCase(m4_s_n1 , Seq(Simplify, Eval), (Double, -1.0)),
    EqualsCompositeTestCase(m4_s_1 , Seq(Simplify, Eval), (Double, 1.0)),
    EqualsCompositeTestCase(m4_s_d0 , Seq(Simplify, Eval), (Double, 0.0)),
  )
}
