package org.combinators.ep.domain.shape   /*DD:LI:AI*/

import org.combinators.ep.domain._

class S0 extends Evolution {

  val domain:BaseDomain = ShapeDomain
  import domain._

  // standard attributes for domain. As new ones are defined, place in respective traits
  val side = Attribute("side", Double)
  val radius = Attribute("radius", Double)
  val x = Attribute("x", Double)
  val y = Attribute("y", Double)
  val trans = Attribute("trans", Point2D)
  val shape = Attribute("shape", baseTypeRep)    // had been Shape
  val point = Parameter("point", Point2D)
  val pct = Parameter("pct", Double)

  case object Double extends TypeRep {
    type scalaInstanceType = scala.Double
  }
  case object Boolean extends TypeRep {
    type scalaInstanceType = scala.Boolean
  }
  case object Point2D extends TypeRep {
    type scalaInstanceType = (scala.Double, scala.Double)
  }

  case object Square extends Atomic("Square", Seq(side))
  case object Circle extends Atomic("Circle", Seq(radius))
  case object Point extends Atomic("Point", Seq(x, y))
  case object Translate extends DataType("Translate", Seq(trans, shape))

  case object ContainsPt extends Operation("containsPt", Boolean, Seq(point))

  case class SquareInst(d:scala.Double) extends AtomicInst(Square, ExistsInstance(Double)(d))
  case class CircleInst(d:scala.Double) extends AtomicInst(Circle, ExistsInstance(Double)(d))
  case class PointInst(x:scala.Double, y:scala.Double) extends AtomicInst(Point, ExistsInstance(Point2D)((x,y)))

  // Not sure if this will work
  case class TranslateInst(pt:(scala.Double,scala.Double), s:Inst) extends Inst(Translate.name) {
    val e: DataType = Translate
    val ei: InstanceModel = ExistsInstance(Point2D)(pt)
  }

  val s0 = Model("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))
  override def getModel:Model = s0

  val sq1 = new SquareInst(5.0)
  val c1 = new CircleInst(5.0)
  val p1:(Double,Double) = (2.0, 2.0)
  val p2:(Double,Double) = (8.0, 0.0)

  val t1 = new TranslateInst((5.0, 7.0), sq1)
  val t2 = new TranslateInst((2.0, -9.0), t1)

  /**
    * Special test case for contains queries.
    *
    * Validates calling containsPt returns true or false
    */
  case class ContainsTestCase(inst:Inst, pt:(Double,Double), result:Boolean)
    extends domain.TestCase {
    val pti: InstanceModel = ExistsInstance(Point2D)(pt)
  }

  def S0_tests:Seq[TestCase] = Seq(
    ContainsTestCase(sq1, p1, result = true),
    ContainsTestCase(c1, p2, result = false),

    ContainsTestCase(t1, p1, result = false),
    ContainsTestCase(t2, p1, result = false),
    ContainsTestCase(t2, p2, result = true),
  )
}
