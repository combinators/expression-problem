package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsCompositeTestCase, EqualsTestCase, Operation, Parameter, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.I1.base
import org.combinators.ep.domain.{Evolution, Model}
import org.combinators.ep.domain.math.M0.{AddInst, Eval, LitInst, DoubleInst}

object M7 extends Evolution {
  override implicit def getModel:Model = M6.getModel.evolve("m7", Seq.empty, Seq(PowBy))

  // m7:model evolution.
  // -------------------
  object m7_extensions {
    val target = "target"
  }

  // add PowBy operation

  // SquareRoot of inner value, and an operation Find that counts the number of occurences
  // in an expression of a target value.
//  lazy val Sqrt:DataTypeCase = DataTypeCase.unary("Sqrt")

  lazy val base = TypeRep.DataType(M2.getModel.baseDataType)
  lazy val PowBy = Operation("powBy", base, Seq(Parameter("other", base)))

//  def SqrtInst(inner:DataTypeInstance): DataTypeInstance =
//    DataTypeInstance(Sqrt, Seq(InstanceRep(inner)))

  object PowByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected:InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (PowBy, Seq(argument)), (Eval, Seq.empty))
    }
  }
 // val m7_1 = SqrtInst(LitInst(25.0))
  val m7_2 = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_3 = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_4 = AddInst(m7_2, m7_3)

  val m7_5 = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_6 = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_7 = AddInst(m7_5, m7_6)

  def tests:Seq[TestCase] = Seq(
   // EqualsTestCase(getModel.baseDataType, m7_1, Eval, M0.DoubleInst(5.0)),
    PowByTestCase(AddInst(LitInst(1.0), LitInst(12.0)),
      InstanceRep(LitInst(4.0)), DoubleInst(13.0*13.0*13.0*13.0))
  )
}
