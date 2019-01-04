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
  val m5_s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(976.0))
  val m5_s2 = new BinaryInst(Add, new LitInst(1.0), new LitInst(976.0))
  val m5_s3 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(976.0))

  // language-specific test cases take advantage of these instances.

//  // generate an expression representing a Tree.
//  def M5_tests:Seq[TestCase] = Seq(
//    EqualsTestCase(m5_s1, AsTree, (Tree, m5_s2)),
//    NotEqualsTestCase(m5_s3, AsTree, (Tree, m5_s2)),
//  )
//    s"""
//       |public void test() {
//       |   assertFalse(${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s2), domain.AsTree)}));
//       |   assertTrue (${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s3), domain.AsTree)}));
//       |}""".stripMargin).methodDeclarations()
}
