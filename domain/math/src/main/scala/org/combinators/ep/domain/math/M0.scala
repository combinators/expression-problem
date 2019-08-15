package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain._

class M0 extends Evolution {

  val domain:BaseDomain = MathDomain
  import domain._

  val litValue = Attribute ("value", Double)

  // m0:model evolution.
  // -------------------
  case object Double extends TypeRep {
    type scalaInstanceType = scala.Double
  }

  case object Lit extends Atomic("Lit", Seq(litValue))
  case object Add extends Binary("Add")

  case object Eval extends Operation("eval", Double)
  case class LitInst(d:scala.Double) extends AtomicInst(Lit, ExistsInstance(Double)(d))

  // TODO: This could be moved until a future evolution. Here to make things easy
  case object Int extends TypeRep  {
    override type scalaInstanceType = scala.Int
  }
  //case object Identifier extends Operation("id", Some(Int))

  val m0 = Model("m0", Seq(Lit, Add), Seq(Eval))
  override def getModel:Model = m0

  // Testing
  def M0_tests:Seq[TestCase] = Seq(
    EqualsTestCase(new BinaryInst(Add, LitInst(1.0), LitInst(2.0)), Eval, ExistsInstance(Double)(3.0)),
    EqualsTestCase(LitInst(5.0), Eval, ExistsInstance(Double)(5.0)),

    PerformanceTestCase(
      11,    // how many iterations to continue the iter
      8,     // how many times to try to find the best
      Eval,
      new BinaryInst(Add, LitInst(1.0), LitInst(2.0)),   // base instance
      Seq.empty,   // base parameters
      params => params,   // how parameters evolve (i.e., stay same)
      inst => new BinaryInst(Add, inst, inst)     // object changes with each iteration
    )
  )
}
