package example.expression.domain  /*DD:LI:AI*/

trait M0 extends Evolution {

  val domain:MathDomain
  import domain._

  val litValue:String = "value"

  // m0:model evolution.
  // -------------------
  case object Double extends TypeRep
  case object Lit extends Atomic("Lit", Seq(Attribute(litValue, Double)))
  case object Add extends Binary("Add")

  case object Eval extends Operation("eval", Some(Double))
  class LitInst(d:Double) extends AtomicInst(Lit, Some(d))

  case object Int extends TypeRep
  case object Identifier extends Operation("id", Some(Int))

  val m0 = Model("m0", Seq(Lit, Add), Seq(Eval, Identifier))
  override def getModel:Model = m0

  // Testing
  def M0_tests:Seq[TestCase] = Seq(
    EqualsTestCase(new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0)), (Double, 3.0), Eval),
    EqualsTestCase(new LitInst(5.0), (Double, 5.0), Eval)
  )
}
