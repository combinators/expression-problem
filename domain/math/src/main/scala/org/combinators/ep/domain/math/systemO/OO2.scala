package org.combinators.ep.domain.math.systemO    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.M2

object OO2 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("oo2", Seq.empty, Seq(InsertM))

  def StringInst(s:String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val InsertM: Operation = Operation("insertm", TypeRep.String)

  // Tests
  val oo2_s1: DataTypeInstance = SubInst(LitInst(1.0), LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, oo2_s1, InsertM, StringInst("(M-M)")),

    EqualsTestCase(getModel.baseDataType, AddInst(oo2_s1, AddInst(LitInst(5.0), LitInst(6.0))),
      InsertM, StringInst("((M-M)+(M+M))"))
  )
}
