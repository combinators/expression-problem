package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, EqualsTestCase, Operation, Parameter, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}

/**
  * Offers a "kitchen sink" of data types and operations envisioned as part of a publication.
  *
  */
object P1 extends Evolution {
  override implicit def getModel:Model = M2.getModel.evolve("p1", Seq(Pi, Rnd, Amortized), Seq(CountBetween, Output, ParamHeight))

  // p1:model evolution.
  // -------------------
  object independent {
    val height:String = "height"
    val P = Attribute("P", TypeRep.DataType(MathDomain.getModel.baseDataType))
    val r = Attribute("r", TypeRep.DataType(MathDomain.getModel.baseDataType))
    val n = Attribute("n", TypeRep.DataType(MathDomain.getModel.baseDataType))
    val countBetween:String = "countBetween"
    val low:String = "low"
    val high:String = "high"
  }

  // This Height implementation takes a parameter, into which the initial call passes the value '0'
  // and then it is passed downwards.
  lazy val ParamHeight = Operation(independent.height, TypeRep.Int, Seq(Parameter(independent.height, TypeRep.Int)))
  lazy val Output = Operation("output")
  lazy val CountBetween = Operation(independent.countBetween, TypeRep.Int,
    Seq(Parameter(independent.low, TypeRep.Double), Parameter(independent.high, TypeRep.Double)),
  )

  lazy val Pi = DataTypeCase.atomic("Pi")
  lazy val Rnd = DataTypeCase.atomic("Rnd")
  lazy val Amortized =  DataTypeCase("Amortized", Seq(independent.P, independent.r, independent.n))

  def AmortizedInst(P:DataTypeInstance, r:DataTypeInstance, n:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Amortized, Seq(InstanceRep(P), InstanceRep(r), InstanceRep(n)))

  val p1_a1 = AmortizedInst(LitInst(100000.0), LitInst(0.06), LitInst(360.0))

  def tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, p1_a1, Eval, InstanceRep(LitInst(599.55)))
  )
}
