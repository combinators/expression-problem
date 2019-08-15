package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain._

/**
  * Offers a "kitchen sink" of data types and operations envisioned as part of a publication.
  *
  */
class P1(val m2:M2) extends Evolution {

  val domain:BaseDomain = MathDomain
  import domain._
  import m2._
  import m1._
  import m0._


  // p1:model evolution.
  // -------------------
  object independent {
    val height:String = "height"
    val P = Attribute("P", baseTypeRep)
    val r = Attribute("r", baseTypeRep)
    val n = Attribute("n", baseTypeRep)
    val countBetween:String = "countBetween"
    val low:String = "low"
    val high:String = "high"
  }

  // This Height implementation takes a parameter, into which the initial call passes the value '0'
  // and then it is passed downwards.
  case object ParamHeight extends Operation(independent.height, Int, Seq(Parameter(independent.height, Int)))
  case object Output extends Operation("output")
  case object CountBetween extends Operation(independent.countBetween, Int,
    Seq(Parameter(independent.low, Double), Parameter(independent.high, Double)))

  case object Pi extends Atomic("Pi", Seq.empty)
  case object Rnd extends Atomic("Rnd", Seq.empty)
  case object Amortized extends Atomic("Amortized",
    Seq(independent.P, independent.r, independent.n)
  )

  // this is how you model an instance with attributes
  class AmortizedInst(override val e:DataType, val P:Inst, val r:Inst, val n:Inst) extends
    NaryInst(e, Seq(P, r, n)) {
  }

  val p1 = Model("p1", Seq(Pi, Rnd, Amortized), Seq(CountBetween, Output, ParamHeight), last = m2.getModel)
  val p1_a1 = new AmortizedInst(Amortized, LitInst(100000.0), LitInst(0.06), LitInst(360.0))

  override def getModel = p1

  def P1_tests:Seq[TestCase] = Seq(
    EqualsTestCase(p1_a1, Eval, ExistsInstance(Double)(599.55)),
  )
}
