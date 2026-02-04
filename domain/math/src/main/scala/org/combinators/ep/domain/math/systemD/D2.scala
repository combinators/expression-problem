package org.combinators.ep.domain.math.systemD    /*DD:LI:AI*/

import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.{M0, M1, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object D2 extends Evolution {
  override implicit def getModel: GenericModel = M1.getModel.evolve("d2", Seq(Mult), Seq.empty)

  lazy val Mult: DataTypeCase = DataTypeCase.binary("Mult")(MathDomain.getModel)

  def MultInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Mult, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  // Tests
  val d2_m1: DataTypeInstance = MultInst(LitInst(2.0), LitInst(3.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M1)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, d2_m1, Eval, M0.DoubleInst(6.0)),
  )
}
