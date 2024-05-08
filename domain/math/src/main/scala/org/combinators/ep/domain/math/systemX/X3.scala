package org.combinators.ep.domain.math.systemX     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.domain.{Evolution, GenericModel}

object X3 extends Evolution {
  override implicit def getModel: GenericModel = X1.getModel.evolve("x3", Seq(Divd), Seq.empty)

  def StringInst(s: String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val Divd = DataTypeCase.binary("Divd")(MathDomain.getModel)

  def DivdInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Divd, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val m2_s1 = SubInst(LitInst(1.0), LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(X1)

  def tests: Seq[TestCase] = Seq(

  )
}
