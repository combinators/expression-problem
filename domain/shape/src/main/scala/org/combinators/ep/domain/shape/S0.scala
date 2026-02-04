package org.combinators.ep.domain.shape   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance

object S0 extends Evolution {

  override implicit def getModel:GenericModel = ShapeDomain.baseModel.evolve("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))

  // standard attributes for domain. As new ones are defined, place in respective traits
  lazy val x = Attribute("x", TypeRep.Double)
  lazy val y = Attribute("y", TypeRep.Double)

  lazy val side = Attribute("side", TypeRep.Double)
  lazy val radius = Attribute("radius", TypeRep.Double)

  lazy val transx = Attribute("transx", TypeRep.Double)
  lazy val transy = Attribute("transy", TypeRep.Double)
  lazy val shape = Attribute("shape", DomainTpeRep.DataType(ShapeDomain.getModel.baseDataType))

  val pointx = Parameter("pointx", TypeRep.Double)
  val pointy = Parameter("pointy", TypeRep.Double)

  lazy val Square = DataTypeCase("Square", Seq(side))
  lazy val Circle = DataTypeCase("Circle", Seq(radius))
  lazy val Translate = DataTypeCase("Translate", Seq(transx, transy, shape))

  lazy val ContainsPt = Operation("containsPt", TypeRep.Boolean, Seq(pointx, pointy))

  def DoubleInst(d:scala.Double):InstanceRep = InstanceRep(TypeRep.Double)(d)

  def SquareInst(d:scala.Double): DataTypeInstance =
    DataTypeInstance(Square, Seq(InstanceRep(TypeRep.Double)(d)))
  def CircleInst(d:scala.Double): DataTypeInstance =
    DataTypeInstance(Circle, Seq(InstanceRep(TypeRep.Double)(d)))
  def PointInst(x:scala.Double, y:scala.Double): DataTypeInstance =
    DataTypeInstance(Circle, Seq(InstanceRep(TypeRep.Double)(x), InstanceRep(TypeRep.Double)(y)))
  def TranslateInst(transx:scala.Double, transy:scala.Double, shape:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Translate, Seq(InstanceRep(TypeRep.Double)(transx), InstanceRep(TypeRep.Double)(transy), DataTypeInstanceRep(shape)))

  val sq1 = SquareInst(5.0)
  val c1 = CircleInst(5.0)
  val p1 = (2.0, 2.0)
  val p2 = (8.0, 0.0)

  val t1 = TranslateInst(5.0, 7.0, sq1)
  val t2 = TranslateInst(2.0, -9.0, t1)

  // all test cases have names so they need
  object ContainsTestCase {
    def apply(instance: DataTypeInstance, p:(scala.Double, scala.Double), result: Boolean): TestCase = {
      EqualsTestCase(
        S0.getModel.baseDataType,
        instance,
        ContainsPt,
        InstanceRep(TypeRep.Boolean)(result),
        InstanceRep(TypeRep.Double)(p._1), InstanceRep(TypeRep.Double)(p._2)
      )
    }
  }

  def tests:Seq[TestCase] = Seq(
 //   EqualsTestCase(getModel.baseDataType, sq1, Unity, DataTypeInstanceRep(TypeRep.Boolean)(true)),
 //   EqualsTestCase(getModel.baseDataType, sq1, ContainsPt, DataTypeInstanceRep(TypeRep.Boolean)(true), DoubleInst(p1._1), DoubleInst(p1._2)),
//    NotEqualsTestCase(getModel.baseDataType, c1, ContainsPt, TRUE, p2._1, p2._2),

    ContainsTestCase(sq1,  p1, result=true),
    ContainsTestCase(c1, p2, result=false),
//
//    ContainsTestCase(t1,  p1, false),
//    ContainsTestCase(t2,  p1, false),
//    ContainsTestCase(t2,  p2, true)
  )
}
