package org.combinators.ep.domain.shape   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}

object S0 extends Evolution {

  override implicit def getModel:Model = ShapeDomain.getModel.evolve("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))

  // standard attributes for domain. As new ones are defined, place in respective traits
  lazy val side = Attribute("side", TypeRep.Double)
  lazy val radius = Attribute("radius", TypeRep.Double)
  lazy val x = Attribute("x", TypeRep.Double)
  lazy val y = Attribute("y", TypeRep.Double)
  lazy val trans = Attribute("trans", Point2D)
  lazy val shape = Attribute("shape", TypeRep.DataType(ShapeDomain.getModel.baseDataType))
  val point = Parameter("point", Point2D)
  val pct = Parameter("pct", TypeRep.Double)

  /** Represents the Scala type `Point` which is (double, double). */
  case object Point2D extends TypeRep {
    type HostType = (scala.Double, scala.Double)
  }

  lazy val Square = DataTypeCase("Square", Seq(side))
  lazy val Circle = DataTypeCase("Circle", Seq(radius))
  lazy val Point = DataTypeCase("Point", Seq(x, y))
  lazy val Translate = DataTypeCase("Translate", Seq(trans, shape))

  lazy val ContainsPt = Operation("containsPt", TypeRep.Boolean, Seq(point))

  def SquareInst(d:scala.Double): DataTypeInstance =
    DataTypeInstance(Square, Seq(InstanceRep(TypeRep.Double)(d)))
  def CircleInst(d:scala.Double): DataTypeInstance =
    DataTypeInstance(Circle, Seq(InstanceRep(TypeRep.Double)(d)))
  def PointInst(x:scala.Double, y:scala.Double): DataTypeInstance =
    DataTypeInstance(Circle, Seq(InstanceRep(TypeRep.Double)(x), InstanceRep(TypeRep.Double)(y)))
  def TranslateInst(trans:(scala.Double, scala.Double), shape:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Square, Seq(InstanceRep(Point2D)(trans), InstanceRep(shape)))

  val sq1 = SquareInst(5.0)
  val c1 = CircleInst(5.0)
  val p1:(Double,Double) = (2.0, 2.0)
  val p2:(Double,Double) = (8.0, 0.0)

  val t1 = TranslateInst((5.0, 7.0), sq1)
  val t2 = TranslateInst((2.0, -9.0), t1)

  /**
    * Special test case for contains queries.
    *
    * Validates calling containsPt returns true or false
    */
  case class ContainsTestCase(
       domainObject: DataTypeInstance,
       op: Operation,
       pt: (Double, Double),
       expected: InstanceRep,
       params: InstanceRep*
     ) extends TestCase

  def tests:Seq[TestCase] = Seq(
    ContainsTestCase(sq1, ContainsPt, p1, InstanceRep(TypeRep.Boolean)(true)),
    ContainsTestCase(c1, ContainsPt, p2, InstanceRep(TypeRep.Boolean)(true)),

    ContainsTestCase(t1, ContainsPt, p1, InstanceRep(TypeRep.Boolean)(false)),
    ContainsTestCase(t2, ContainsPt, p1, InstanceRep(TypeRep.Boolean)(false)),
    ContainsTestCase(t2, ContainsPt, p2, InstanceRep(TypeRep.Boolean)(true)),
  )
}
