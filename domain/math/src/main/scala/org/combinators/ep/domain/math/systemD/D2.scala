package org.combinators.ep.domain.math.systemD    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.{M0, M1, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object D2 extends Evolution {
  override implicit def getModel: GenericModel = M1.getModel.evolve("d2", Seq(Mult), Seq.empty)

  // m3:model evolution.
  // -------------------
  lazy val Mult = DataTypeCase.binary("Mult")(MathDomain.getModel)

  def MultInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Mult, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val m3_s1 = MultInst(LitInst(2.0), LitInst(3.0))

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m3_s1, Eval, M0.DoubleInst(6.0)),
  )
}
