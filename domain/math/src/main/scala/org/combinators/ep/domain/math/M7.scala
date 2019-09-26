package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, Operation, Parameter, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.{Evolution, Model}
import org.combinators.ep.domain.math.M0.{AddInst, Eval, LitInst}

object M7 extends Evolution {
  override implicit def getModel:Model = M6.getModel.evolve("m7", Seq(Sqrt), Seq(Find))

  // m7:model evolution.
  // -------------------
  object m7_extensions {
    val target = "target"
  }

  // SquareRoot of inner value, and an operation Find that counts the number
  lazy val Sqrt:DataTypeCase = DataTypeCase.unary("Sqrt")

  lazy val Find = Operation("find", TypeRep.Int, Seq(Parameter(m7_extensions.target, TypeRep.Double)))

  def SqrtInst(inner:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Sqrt, Seq(InstanceRep(inner)))


  val m7_1 = SqrtInst(LitInst(25.0))
  val m7_2 = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_3 = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_4 = AddInst(m7_2, m7_3)

  val m7_5 = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_6 = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_7 = AddInst(m7_5, m7_6)

  def tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m7_1, Eval, InstanceRep(LitInst(5.0))),
    EqualsTestCase(getModel.baseDataType, m7_4, Find, InstanceRep(LitInst(1.0)), InstanceRep(TypeRep.Int)(2)),
  )
}
