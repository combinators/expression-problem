package org.combinators.ep.domain.math.systemJK    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{EqualsTestCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}
import org.combinators.ep.domain.math.systemJ.J1.SubInst
import org.combinators.ep.domain.math.systemJ.J2.MultInst
import org.combinators.ep.domain.math.systemJ.J3.{DivdInst, NegInst}
import org.combinators.ep.domain.math.systemJK.J7.InvInst
import org.combinators.ep.domain.math.systemK.K1.PowerInst
import org.combinators.ep.domain.{Evolution, GenericModel}

object J8 extends Evolution {
  override implicit def getModel: GenericModel = J7.getModel.evolve("j8", Seq.empty, Seq(Height))

  lazy val Height = Operation("height", TypeRep.Int, Seq.empty)

  //      m9_3                 <-- m9_3 has height of 3
  //      /  \
  //   m9_0   m9_2             <-- m9_2 has height of 2
  //           / \
  //        m9_0  m9_1         <-- m9_1 has height of 1
  //               / \
  //           m9_0   m9_0     <-- height(s) of 0
  //
  // and also do right-leaning...
  val m9_0 = LitInst(2.0)
  val m9_1 = AddInst(m9_0, m9_0)
  val m9_2 = SubInst(m9_0, m9_1)
  val m9_3 = DivdInst(m9_0, m9_2)
  val m9_1r = AddInst(m9_0, m9_0)
  val m9_2r = SubInst(m9_0, m9_1r)
  val m9_3r = DivdInst(m9_2r, m9_0)

  val ma_1 = InvInst(m9_0, m9_0)
  val ma_2 = MultInst(m9_0, ma_1)
  val ma_3 = DivdInst(m9_0, ma_2)
  val ma_4 = NegInst(ma_3)
  val ma_5 = PowerInst(ma_4, ma_4)
  val ma_2r = MultInst(m9_0, ma_1)
  val ma_3r = DivdInst(m9_0, ma_2r)
  val ma_4r = NegInst(ma_3r)
  val ma_5r = PowerInst(ma_4r, ma_4r)

  def IntInst(i: scala.Int): InstanceRep =
    InstanceRep(TypeRep.Int)(i)

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(J7)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m9_0, Height, IntInst(0)),
    EqualsTestCase(getModel.baseDataType, m9_1, Height, IntInst(1)),
    EqualsTestCase(getModel.baseDataType, m9_2, Height, IntInst(2)),
    EqualsTestCase(getModel.baseDataType, m9_3, Height, IntInst(3)),

    EqualsTestCase(getModel.baseDataType, m9_1r, Height, IntInst(1)),
    EqualsTestCase(getModel.baseDataType, m9_2r, Height, IntInst(2)),
    EqualsTestCase(getModel.baseDataType, m9_3r, Height, IntInst(3)),

    EqualsTestCase(getModel.baseDataType, ma_1, Height, IntInst(1)),
    EqualsTestCase(getModel.baseDataType, ma_2, Height, IntInst(2)),
    EqualsTestCase(getModel.baseDataType, ma_3, Height, IntInst(3)),
    EqualsTestCase(getModel.baseDataType, ma_4, Height, IntInst(4)),
    EqualsTestCase(getModel.baseDataType, ma_5, Height, IntInst(5)),
    EqualsTestCase(getModel.baseDataType, ma_2r, Height, IntInst(2)),
    EqualsTestCase(getModel.baseDataType, ma_3r, Height, IntInst(3)),
    EqualsTestCase(getModel.baseDataType, ma_4r, Height, IntInst(4)),
    EqualsTestCase(getModel.baseDataType, ma_5r, Height, IntInst(5)),
  )
}
