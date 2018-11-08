package example.expression.domain  /*DD:LI:AI*/

trait M5 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 =>
  val domain:MathDomain
  import domain._

  // m5:model evolution.
  // -------------------
  // Represent structure as a tree 

  val m5 = Model("m5", Seq.empty, Seq(domain.AsTree), last = m4)
  override def getModel = m5

  // Tests
  val m5_s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
  val s2 = new BinaryInst(Sub, new LitInst(9.0), new LitInst(112.0))
  val m5_s2 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

  // two trees can be compared for being 'same'
  case class SameTestCase(override val inst:AtomicInst, override val expect:(TypeRep,Any), override val op:Operation) extends TestCase (inst, expect, op)

  def M5_tests:Seq[TestCase] = Seq(
    EqualsTestCase(m5_s1, (Tree, m5_s2), AsTree),
    EqualsTestCase(m3_m1, (String, "-1.0"), PrettyP),

    EqualsTestCase(m4_m1, (String, "((5.0/2.0)*4.0)"), PrettyP),
    EqualsTestCase(m4_m1, (Double, 10.0), Eval)
  )
//    s"""
//       |public void test() {
//       |   assertFalse(${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s2), domain.AsTree)}));
//       |   assertTrue (${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s3), domain.AsTree)}));
//       |}""".stripMargin).methodDeclarations()
}
