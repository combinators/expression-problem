package org.combinators.ep.domain.math.systemX     /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{TestCase, TypeRep}
import org.combinators.ep.domain.abstractions.DataTypeCase
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.domain.{Evolution, GenericModel}

object X3 extends Evolution {
  override implicit def getModel: GenericModel = X1.getModel.evolve("x3", Seq(Divd), Seq.empty)

  def StringInst(s: String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val Divd: DataTypeCase = DataTypeCase.binary("Divd")(MathDomain.getModel)

  def DivdInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Divd, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  // Tests
  val m2_s1: DataTypeInstance = SubInst(LitInst(1.0), LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(X1)

  def tests: Seq[TestCase] = Seq(

  )
}
