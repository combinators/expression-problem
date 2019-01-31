package ep.domain   /*DD:LI:AI*/

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

  /**
    * Special test case for binary methods.
    *
    * Validates that calling AsTree on inst1 yields the tree called from AsTree on inst2
    */
  case class EqualsBinaryMethodTestCase(inst1:domain.AtomicInst, inst2:domain.AtomicInst, result:Boolean)
    extends domain.TestCase

  val m6_s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(73.0))
  val m6_s2 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(73.0))
  val m6_s3 = new BinaryInst(Add, new LitInst(5.0), new LitInst(3.0))

  val m6_m1 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(4.0))
  val m6_m2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(3.0))
  val m6_m3 = new domain.UnaryInst(Neg, m6_m1)

  val m6_d3 = new BinaryInst(Divd, new LitInst(6.0), new LitInst(2.0))
  val m6_d4 = new BinaryInst(Divd, new LitInst(8.0), new LitInst(2.0))

  def M6_tests:Seq[TestCase] = Seq(
    EqualsBinaryMethodTestCase(m6_s2, m6_s1, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m2, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m1, result=true),  // parameter to operation

    EqualsBinaryMethodTestCase(m6_m3, m6_m3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m3, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_d3, m6_d4, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_d3, m6_d3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_s3, m6_s3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_s3, m6_m2, result=false),  // parameter to operation
  )
}
