package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{EqualsTestCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}

object M9 extends Evolution {
  override implicit def getModel:GenericModel = M8.getModel.evolve("m9", Seq.empty, Seq(Height))

  lazy val Height = Operation("height", TypeRep.Int, Seq.empty)

  //      m9_3                 <-- m9_3 has height of 3
  //      /  \
  //   m9_0   m9_2             <-- m9_2 has height of 2
  //           / \
  //        m9_0  m9_1         <-- m9_1 has height of 1
  //               / \
  //           m9_0   m9_0     <-- height(s) of 0
  //
  val m9_0 = LitInst(2.0)
  val m9_1 = AddInst(m9_0, m9_0)
  val m9_2 = AddInst(m9_0, m9_1)
  val m9_3 = AddInst(m9_0, m9_2)

  def IntInst(i: scala.Int): InstanceRep =
    InstanceRep(TypeRep.Int)(i)

  // couldn't flatten, since the generated file typically exceeds byte-code limit of comp units
  // can work out solution within Java test generator
  override def allTests: Map[GenericModel, Seq[TestCase]] = M8.allTests +
    (getModel -> tests)

  def tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m9_0, Height, IntInst(0)),
    EqualsTestCase(getModel.baseDataType, m9_1, Height, IntInst(1)),
    EqualsTestCase(getModel.baseDataType, m9_2, Height, IntInst(2)),
    EqualsTestCase(getModel.baseDataType, m9_3, Height, IntInst(3)),
  )
}
