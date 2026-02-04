package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{TestCase, TypeRep}
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, DomainTpeRep, EqualsTestCase, Operation, Parameter}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}

/**
  * Offers a "kitchen sink" of data types and operations envisioned as part of a publication.
  * NEVER IMPLEMENTED
  */
object P1 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("p1", Seq(Pi, Rnd, Amortized), Seq(CountBetween, Output, ParamHeight))

  object independent {
    val height: String = "height"
    val P: Attribute = Attribute("P", DomainTpeRep.DataType(MathDomain.getModel.baseDataType))
    val r: Attribute = Attribute("r", DomainTpeRep.DataType(MathDomain.getModel.baseDataType))
    val n: Attribute = Attribute("n", DomainTpeRep.DataType(MathDomain.getModel.baseDataType))
    val countBetween: String = "countBetween"
    val low: String = "low"
    val high: String = "high"
  }

  // This Height implementation takes a parameter, into which the initial call passes the value '0'
  // and then it is passed downwards.
  lazy val ParamHeight: Operation = Operation(independent.height, TypeRep.Int, Seq(Parameter(independent.height, TypeRep.Int)))
  lazy val Output: Operation = Operation("output")
  lazy val CountBetween: Operation = Operation(independent.countBetween, TypeRep.Int,
    Seq(Parameter(independent.low, TypeRep.Double), Parameter(independent.high, TypeRep.Double)),
  )

  lazy val Pi: DataTypeCase = DataTypeCase.atomic("Pi")
  lazy val Rnd: DataTypeCase = DataTypeCase.atomic("Rnd")
  lazy val Amortized: DataTypeCase = DataTypeCase("Amortized", Seq(independent.P, independent.r, independent.n))

  def AmortizedInst(P:DataTypeInstance, r:DataTypeInstance, n:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Amortized, Seq(DataTypeInstanceRep(P), DataTypeInstanceRep(r), DataTypeInstanceRep(n)))

  val p1_a1: DataTypeInstance = AmortizedInst(LitInst(100000.0), LitInst(0.06), LitInst(360.0))

  def tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, p1_a1, Eval, DataTypeInstanceRep(LitInst(599.55)))
  )
}
