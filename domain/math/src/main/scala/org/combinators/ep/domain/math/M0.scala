package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait M0 extends Evolution {

  val domain:MathDomain
  import domain._

  val litValue = Attribute ("value", Double)

  // m0:model evolution.
  // -------------------
  case object Double extends TypeRep {
    type scalaInstanceType = scala.Double
  }

  case object Lit extends Atomic("Lit", Seq(litValue))
  case object Add extends Binary("Add")

  case object Eval extends Operation("eval", Some(Double))
  class LitInst(d:scala.Double) extends AtomicInst(Lit, ExistsInstance(Double)(d))

  case object Int extends TypeRep  {
    override type scalaInstanceType = scala.Int
  }
  case object Identifier extends Operation("id", Some(Int))

  val m0 = Model("m0", Seq(Lit, Add), Seq(Eval, Identifier))
  override def getModel:Model = m0

  // Testing
  def M0_tests:Seq[TestCase] = Seq(
    EqualsTestCase(new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0)), Eval, ExistsInstance(Double)(3.0)),
    EqualsTestCase(new LitInst(5.0), Eval, ExistsInstance(Double)(5.0))
  )
}
