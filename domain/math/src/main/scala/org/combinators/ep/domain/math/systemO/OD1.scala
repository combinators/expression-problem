package org.combinators.ep.domain.math.systemO  /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.{M2, MathDomain}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

object OD1 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("od1", Seq(Mult), Seq.empty)

  lazy val Mult: DataTypeCase = DataTypeCase.binary("Mult")(MathDomain.getModel)

  def MultInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Mult, Seq(InstanceRep(left), InstanceRep(right)))

  val od1: DataTypeInstance = MultInst(LitInst(2.0), LitInst(3.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, od1, PrettyP, StringInst("(2.0*3.0)")),
  )
}
