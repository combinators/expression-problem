package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.math.{M2, MathDomain}

object OD2 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("od2", Seq(Divd), Seq.empty)

  lazy val Divd: DataTypeCase = DataTypeCase.binary("Divd")(MathDomain.getModel)

  def DivdInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Divd, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  val od2: DataTypeInstance = DivdInst(LitInst(2.0), LitInst(3.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, od2, PrettyP, StringInst("(2.0/3.0)")),
  )
}
