package org.combinators.ep.domain.shape   /*DD:LI:AI*/


import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.shape.S0.*

object S1 extends Evolution {
  override implicit def getModel:GenericModel = S0.getModel.evolve("s1", Seq(Union), Seq.empty)

  lazy val s1 = Attribute("s1", DomainTpeRep.DataType(ShapeDomain.getModel.baseDataType))
  lazy val s2 = Attribute("s2", DomainTpeRep.DataType(ShapeDomain.getModel.baseDataType))

  lazy val Union = DataTypeCase("Union", Seq(s1, s2))

  def UnionInst(i1:DataTypeInstance, i2:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Union, Seq(DataTypeInstanceRep(i1), DataTypeInstanceRep(i2)))

  // point IN circle but not in SQUARE [radius = 5
  val p3 = (3.0, 3.0)

  def tests: Seq[TestCase] = Seq(
    ContainsTestCase(UnionInst(sq1, c1),  p1, result = true),
    ContainsTestCase(UnionInst(sq1, c1),  p3, result = true),
    ContainsTestCase(sq1,  p3, result = false),
  )
}
