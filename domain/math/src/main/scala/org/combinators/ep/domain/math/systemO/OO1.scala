package org.combinators.ep.domain.math.systemO    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.M2

object OO1 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("oo1", Seq.empty, Seq(Atomic))

  def StringInst(s:String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val Atomic = Operation("atomic", TypeRep.String)

  // Tests
  val oo1_s1: DataTypeInstance = SubInst(LitInst(1.0), LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, oo1_s1, Atomic, StringInst("(L-L)")),

    EqualsTestCase(getModel.baseDataType, AddInst(oo1_s1, AddInst(LitInst(5.0), LitInst(6.0))),
      Atomic, StringInst("((L-L)+(L+L))"))
  )

}
