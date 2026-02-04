package org.combinators.ep.domain.math.systemO  /*DD:LI:AI*/

import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.{M2, MathDomain}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

object OD1 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("od1", Seq(Mult), Seq.empty)

  lazy val Mult: DataTypeCase = DataTypeCase.binary("Mult")(MathDomain.getModel)

  def MultInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Mult, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  val od1: DataTypeInstance = MultInst(LitInst(2.0), LitInst(3.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, od1, PrettyP, StringInst("(2.0*3.0)")),
  )
}
