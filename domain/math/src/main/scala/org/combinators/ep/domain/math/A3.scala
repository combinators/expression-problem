package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

/**
                    ---------------------- i2 [Power]
     [sub]         /                       |
m0 <- m1 <- m2 <- i1 <- a1 [Mult]          |
           [Pp]  [MultBy]    |             |
            |                |             |
            |                |             |
           m3 <------------ a1m3 <------ a1m3i2 <- a3 [Div]
         [PrettyP]


 */
object A3 extends Evolution {
  override implicit def getModel:GenericModel = A1M3I2.getModel.evolve("a3", Seq(Inv), Seq.empty)

  // A3:model evolution.  INV which is opposite of DIV
  // -------------------
  lazy val Inv = DataTypeCase.binary("Inv")(MathDomain.getModel)

  def DivdInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Inv, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val m3_m2_d1 = DivdInst(LitInst(5.0),  LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(A1M3I2)


  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, m3_m2_d1, PrettyP, StringInst("(2.0/5.0)")),
    EqualsTestCase(getModel.baseDataType, m3_m2_d1, Eval, M0.DoubleInst(0.4)),
  )
}

